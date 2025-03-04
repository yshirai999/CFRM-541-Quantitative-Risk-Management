import numpy as np
from scipy.io import loadmat
import pandas as pd
import matplotlib.pyplot as plt

class data():

    def __init__(self, tickers = ['spy', 'xle', 'xlf', 'xli', 'xlk', 'xlp', 'xlu', 'xlv', 'xly', 'xom', 'xrx']) -> None:
        self.tickers = tickers
        mat = loadmat('tsd180.mat')
        self.d = mat['days']
        self.p = mat['pm']
        self.n = mat['nmss']
        [N,M] = np.shape(self.p)
        self.N = N
        self.M = M
        self.Data = pd.DataFrame({'days': self.d[:,0]})
        for i in range(M):
            Datai = pd.DataFrame({self.n[i,0][0] : self.p[:,i]})
            self.Data = pd.concat([self.Data,Datai],axis=1)

        self.DataETFs = self.Data[self.tickers]
        self.DataETFsReturns = self.DataETFs.diff()
        self.DataETFsReturns = self.DataETFsReturns.drop(index = 0)
        self.DataETFsReturns.insert(0, 'days', self.d[1:])
        self.DataETFsAbsReturns = self.DataETFsReturns[self.tickers].abs()
        self.DataETFsAbsReturns.insert(0, 'days', self.d[1:])
        self.DataETFsReturns['days'] = pd.to_datetime(self.DataETFsReturns['days'], format='%Y%m%d')
        self.DataETFsAbsReturns['days'] = pd.to_datetime(self.DataETFsReturns['days'], format='%Y%m%d')

    def Returns_Visualization(self, ticker: str, dataframe: pd.DataFrame) -> None:
        if ticker not in self.tickers:
             print('Error: ticker should be one of:', self.tickers)
             return
        x = dataframe['days']
        y = dataframe[ticker]

        # Create the plot
        plt.figure(figsize=(10, 5))
        plt.plot(x, y, label='spy', color='blue', linestyle='-', linewidth=0.1, marker='o', markersize=1)

        # Add labels and title
        plt.xlabel('x-axis')
        plt.ylabel('y-axis')
        plt.title('SPY daily return')

        # Add legend
        plt.legend()

        # Add grid
        plt.grid(True)

        # Show the plot
        plt.show()



