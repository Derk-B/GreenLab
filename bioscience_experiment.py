import bioscience as bs
import gc
import sys

class BioScienceExperiment:
    def __init__(self, dataset_path: str, mode: int):
        self.dataset_path = dataset_path
        self.mode = mode
        self.dataset = None
        self.finished = False
    
    def load(self):
        self.dataset = bs.load(path=self.dataset_path, index_gene=0, naFilter=False, head=0)

    def run(self):
        if self.dataset is None:
            self.load()
        
        listModels = bs.bibit(self.dataset, cMnr=2, cMnc=2, mode=self.mode, debug=True)
        
        self.finished = True
        
        del self.dataset
        del listModels
        self.dataset = None
        gc.collect()

    def is_finished(self) -> bool:
        """Check if the experiment has finished."""
        return self.finished

def main():
    if len(sys.argv) < 3:
        print("Usage: python bioscience_experiment.py <dataset_path> <mode>")
        sys.exit(1)

    dataset_path = sys.argv[1]
    mode = int(sys.argv[2])
    experiment = BioScienceExperiment(dataset_path, mode)
    experiment.load()
    experiment.run()

if __name__ == "__main__":
    main()
