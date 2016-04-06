using NeuralNetwork;

namespace NeuralNetworkClient
{
    class Client
    {
        static void Main()
        {
            int[] layerSizes = new int[3] { 1, 2, 1 };
            TransferFunctionEnum[] tFuncs = new TransferFunctionEnum[3]
            {
                TransferFunctionEnum.None,
                TransferFunctionEnum.Sigmoid,
                TransferFunctionEnum.Sigmoid
            };
            BackPropagationNetwork bpn = new BackPropagationNetwork(layerSizes, tFuncs);
        }
    }
}
