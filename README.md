# Graph Convolutional Neural Network (GCN) for Power System Line Outage Identification
The proposed GCN models are applied to the task of power system line outage identification using Tensorflow. However it can be employed to classify generel network signal pattern. 
## Data 
To implement the model, you need to provide
- Input data with size N*F. N is the number of nodes in the network, F is the number of features per node.
- N*N adjacency matrix or Laplacian matrix.
- Output label of each input.

## Models
Based on different signal filtering strategies, three GCN models are proposed,
- Spatial Convolution. Filtering signal in vertex domain.
- Spectral Convolution. Filtering signal in spectral domain via graph Fourier transform.
- Spectral-B Convolution. This is theoretically equivalent to Spatial Convolution.

