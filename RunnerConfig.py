from EventManager.Models.RunnerEvents import RunnerEvents
from EventManager.EventSubscriptionController import EventSubscriptionController
from ConfigValidator.Config.Models.RunTableModel import RunTableModel
from ConfigValidator.Config.Models.FactorModel import FactorModel
from ConfigValidator.Config.Models.RunnerContext import RunnerContext
from ConfigValidator.Config.Models.OperationType import OperationType
from ExtendedTyping.Typing import SupportsStr
from ProgressManager.Output.OutputProcedure import OutputProcedure as output

from typing import Dict, List, Any, Optional
import pandas as pd
from pathlib import Path
from os.path import dirname, realpath

import subprocess
import shlex
import os
import time
import sys
import gc

class RunnerConfig:
    ROOT_DIR = Path(dirname(realpath(__file__)))

    # ================================ USER SPECIFIC CONFIG ================================
    """The name of the experiment."""
    name:                       str             = "biclustering_experiment_full"

    """The path in which Experiment Runner will create a folder with the name `self.name`, in order to store the
    results from this experiment. (Path does not need to exist - it will be created if necessary.)
    Output path defaults to the config file's path, inside the folder 'experiments'"""
    results_output_path:        Path            = ROOT_DIR / 'experiments'

    """Experiment operation type. Unless you manually want to initiate each run, use `OperationType.AUTO`."""
    operation_type:             OperationType   = OperationType.AUTO

    """The time Experiment Runner will wait after a run completes.
    This can be essential to accommodate for cooldown periods on some systems."""
    time_between_runs_in_ms:    int             = 60000

    # Dynamic configurations can be one-time satisfied here before the program takes the config as-is
    # e.g. Setting some variable based on some criteria
    def __init__(self):
        """Executes immediately after program start, on config load"""

        EventSubscriptionController.subscribe_to_multiple_events([
            (RunnerEvents.BEFORE_EXPERIMENT, self.before_experiment),
            (RunnerEvents.BEFORE_RUN       , self.before_run       ),
            (RunnerEvents.START_RUN        , self.start_run        ),
            (RunnerEvents.START_MEASUREMENT, self.start_measurement),
            (RunnerEvents.INTERACT         , self.interact         ),
            (RunnerEvents.STOP_MEASUREMENT , self.stop_measurement ),
            (RunnerEvents.STOP_RUN         , self.stop_run         ),
            (RunnerEvents.POPULATE_RUN_DATA, self.populate_run_data),
            (RunnerEvents.AFTER_EXPERIMENT , self.after_experiment )
        ])
        self.run_table_model = None  # Initialized later

        output.console_log("Custom config loaded")

    def create_run_table_model(self) -> RunTableModel:
        """Create and return the run_table model here. A run_table is a List (rows) of tuples (columns),
        representing each run performed"""
        dataset_factor = FactorModel("dataset", ['GDS3900_CPM.csv', 'GDS3900_TPM.csv',  'low_spar_TPM.csv', 'high_spar_TPM.csv', 'high_spar_CPM.csv', 'low_spar_CPM.csv'])
        # algorithm_factor = FactorModel("algorithm", ['DESeq2', 'TPM', 'FPKM'])
        hardware_factor = FactorModel("hardware", ['CPU', 'GPU'])

        self.run_table_model = RunTableModel(
            factors=[dataset_factor, hardware_factor],
            exclude_variations=[],
            repetitions = 30,
            data_columns=['execution_time_(ms)', 'peak_memory_(B)', 'energy_consumption_(J)', 'cpu_energy_(J)', 'gpu_energy_(J)','mean_cpu_utilization_(%)', 'mean_gpu_utilization_(%)','max_cpu_utilization_(%)','max_gpu_utilization_(%)']
        )
        return self.run_table_model

    def before_experiment(self) -> None:
        """Perform any activity required before starting the experiment here
        Invoked only once during the lifetime of the program."""

        output.console_log("Initializing resources for the experiment.")

    def before_run(self) -> None:
        """Perform any activity required before starting a run.
        No context is available here as the run is not yet active (BEFORE RUN)"""

        output.console_log("Preparing for the next run.")

    def start_run(self, context: RunnerContext) -> None:
        output.console_log("Starting biclustering algorithm.")
        self.dataset = context.run_variation['dataset']
        self.hardware = context.run_variation['hardware']
        self.mode = 2 if self.hardware == 'CPU' else 3


    def start_measurement(self, context: RunnerContext) -> None:
        """Perform any activity required for starting measurements."""
        sampling_interval = 200
        dataset_path = f"/path/to/dataset{self.dataset}"
        script_path = "/path/to/script"

        profiler_cmd = f'energibridge \
                        -g \
                        --interval {sampling_interval} \
                        --output {context.run_dir / "energibridge.csv"} \
                        --summary \
                        python3 {script_path} {dataset_path} {self.mode}'
        energibridge_log = open(f'{context.run_dir}/energibridge.log', 'w')
        self.profiler = subprocess.Popen(shlex.split(profiler_cmd), stdout=energibridge_log)

        output.console_log(f"Starting experiment")

    def interact(self, context: RunnerContext) -> None:
        """Perform any interaction with the running target system here, or block here until the target finishes."""

        stdout, stderr = self.profiler.communicate()
        gc.collect()

        if stdout:
            output.console_log(f"Combined command completed successfully: {stdout.decode().strip()}")
        else:
            output.console_log("Combined command completed with no standard output.")

        if stderr:
            output.console_log(f"Combined command stderr: {stderr.decode().strip()}")

        if self.profiler.returncode != 0:
            output.console_log(f"Combined command failed with error: {stderr.decode().strip()}" if stderr else "Combined command failed with no error message.")
        else:
            output.console_log("Experiment finished. Starting cooldown period.")
            time.sleep(self.time_between_runs_in_ms / 1000.0)  

    def stop_measurement(self, context: RunnerContext) -> None:
        """Perform any activity here required for stopping measurements."""

        output.console_log("Config.stop_measurement called!")
        self.profiler.terminate()
        gc.collect()

    def stop_run(self, context: RunnerContext) -> None:
        """Perform any activity here required for stopping the run.
        Activities after stopping the run should also be performed here."""
        self.profiler.kill()
        self.profiler = None
        output.console_log("Profiler killed")
        gc.collect()
        output.console_log(f"profiler value {self.profiler}")
        output.console_log("Config.stop_run() called!")

    def populate_run_data(self, context: RunnerContext) -> Optional[Dict[str, SupportsStr]]:
        """Parse and process any measurement data here.
        You can also store the raw measurement data under `context.run_dir`
        Returns a dictionary with keys `self.run_table_model.data_columns` and their values populated"""
        output.console_log("Collecting and processing run data.")
        df = pd.read_csv(context.run_dir / f"energibridge.csv")

        if 'USED_MEMORY' in df.columns:
            peak_memory = df['USED_MEMORY'].max() 
        else:
            output.console_log("'USED_MEMORY' column not found in CSV.")
            peak_memory = None

        cpu_energy =  df['CPU_ENERGY (J)'].iloc[-1] - df['CPU_ENERGY (J)'].iloc[0] 
        gpu_power_mW = df['GPU0_POWER (mWatts)'].iloc[-1] - df['GPU0_POWER (mWatts)'].iloc[0] 

        mean_cpu_utilization = df[[col for col in df.columns if 'CPU_USAGE' in col]].mean().mean() if any('CPU_USAGE' in col for col in df.columns) else None
        mean_gpu_utilization = df['GPU0_USAGE'].mean() if 'GPU0_USAGE' in df.columns else None

        max_cpu_utilization = df[[col for col in df.columns if 'CPU_USAGE' in col]].max().max() if any('CPU_USAGE' in col for col in df.columns) else None
        max_gpu_utilization = df['GPU0_USAGE'].max() if 'GPU0_USAGE' in df.columns else None

        if 'Time' in df.columns:
            run_time_ms = df['Time'].iloc[-1] - df['Time'].iloc[0] 
        else:
            output.console_log("'Time' column not found in CSV.")
            run_time_ms = None

        if gpu_power_mW is not None and run_time_ms is not None:
            gpu_energy_J = gpu_power_mW * (run_time_ms / 1000) * 1e-3 
        else:
            gpu_energy_J = None

        if gpu_energy_J < 0:
            gpu_energy_J = 0
            
        run_data = {
            'execution_time_(ms)': run_time_ms,
            'peak_memory_(B)': peak_memory,
            'energy_consumption_(J)': cpu_energy + gpu_energy_J,
            'cpu_energy_(J)': cpu_energy,
            'gpu_energy_(J)': gpu_energy_J,
            'mean_cpu_utilization_(%)': mean_cpu_utilization,
            'mean_gpu_utilization_(%)': mean_gpu_utilization,
            'max_cpu_utilization_(%)': max_cpu_utilization,
            'max_gpu_utilization_(%)': max_gpu_utilization
        }
        return run_data

    def after_experiment(self) -> None:
        """Perform any activity required after stopping the experiment here
        Invoked only once during the lifetime of the program."""
        gc.collect()
        output.console_log("Config.after_experiment() called!")

    # ================================ DO NOT ALTER BELOW THIS LINE ================================
    experiment_path:            Path             = None
