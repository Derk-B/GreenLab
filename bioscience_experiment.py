import bioscience as bs
import gc
import sys
import pandas as pd

class BioScienceExperiment:
    def __init__(self, dataset_path: str, mode: int):
        self.dataset_path = dataset_path
        self.mode = mode
        self.dataset = None
        self.finished = False
    
    def load(self):
        data = pd.read_csv(self.dataset_path, sep=',').values
        lengths = None
        geneNames = None
        colNames = [str(i) for i in range(len(data[0]))]
        
        self.dataset = bs.Dataset(data, geneNames=geneNames, columnsNames=colNames, lengths=lengths)
        # self.dataset = bs.load(path=self.dataset_path, index_gene=0, naFilter=False, head=0)

    def run(self):
        if self.dataset is None:
            self.load()
            
        # bs.tpm(self.dataset)
        # bs.binarize(self.dataset)
        
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
    print("ARGS: ", sys.argv)
    if len(sys.argv) < 3:
        print("Usage: python bioscience_experiment.py <dataset_path> <mode>")
        sys.exit(1)

    dataset_path = sys.argv[1]
    mode = int(sys.argv[2])
    print(dataset_path, mode)
    # mode = 3
    experiment = BioScienceExperiment(dataset_path, mode)
    experiment.load()
    experiment.run()

if __name__ == "__main__":
    main()