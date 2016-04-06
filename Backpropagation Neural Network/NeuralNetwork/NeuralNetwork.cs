using System;

namespace NeuralNetwork
{
    #region Transfer functions and their derivatives

    public enum TransferFunctionEnum
    {
        None,
        Sigmoid
    }

    public static class TransferFunction
    {
        public static double GetSigmoid(TransferFunctionEnum tFunc, double input)
        {
            switch (tFunc)
            {
                case TransferFunctionEnum.Sigmoid:
                    return CalculateSigmoid(input);
                case TransferFunctionEnum.None:
                default:
                    return 0.0;
            }
        }

        public static double GetSigmoidDerivative(TransferFunctionEnum tFunc, double input)
        {
            switch (tFunc)
            {
                case TransferFunctionEnum.Sigmoid:
                    return CalculateSigmoidDerivative(input);
                case TransferFunctionEnum.None:
                default:
                    return 0.0;
            }
        }
        
        private static double CalculateSigmoid(double x)
        {
            double result = 1.0 / (1.0 + Math.Exp(x));

            return result;
        }

        private static double CalculateSigmoidDerivative(double x)
        {
            double result = CalculateSigmoid(x) * (1 - CalculateSigmoid(x));

            return result;
        }
    }

    #endregion

    public class BackPropagationNetwork
    {
        #region Private data

        private int layerCount;
        private int inputSize;
        private int[] layerSize;
        private TransferFunctionEnum[] transferFunction;

        private double[][] layerOutput;
        private double[][] layerInput;
        private double[][] bias;
        private double[][] delta;
        private double[][] previousBiasDelta;

        private double[][][] weight;
        private double[][][] previousWeightDelta;

        #endregion

        #region Constructors

        public BackPropagationNetwork(int[] layerSizes, TransferFunctionEnum[] transferFunctions)
        {
            // Input validation
            if (transferFunctions.Length != layerSizes.Length || transferFunctions[0] != TransferFunctionEnum.None)
            {
                throw new ArgumentException("Cannot construct a network with these parameters.");
            }

            // Layer initializaztion
            this.layerCount = layerSizes.Length - 1; // excluding the input layer
            this.layerSize = new int[layerCount];
            this.inputSize = layerSizes[0];

            for (int i = 0; i < layerCount; i++)
            {
                layerSize[i] = layerSizes[i + 1];
            }

            this.transferFunction = new TransferFunctionEnum[this.layerCount];

            for (int i = 0; i < this.layerCount; i++)
            {
                this.transferFunction[i] = transferFunctions[i + 1];
            }

            // Array dimensioning
            this.bias = new double[this.layerCount][];
            this.previousBiasDelta = new double[this.layerCount][];
            this.layerOutput = new double[this.layerCount][];
            this.layerInput = new double[this.layerCount][];

            this.weight = new double[this.layerCount][][];
            this.previousWeightDelta = new double[this.layerCount][][];

            // 2D array initialization
            for (int l = 0; l < this.layerCount; l++)
            {
                this.bias[l] = new double[this.layerSize[l]];
                this.previousBiasDelta[l] = new double[this.layerSize[l]];
                this.layerOutput[l] = new double[this.layerSize[l]];
                this.layerInput[l] = new double[this.layerSize[l]];

                this.weight[l] = new double[(l == 0) ? this.inputSize : this.layerSize[l - 1]][];
                this.previousWeightDelta[l] = new double[(l == 0) ? this.inputSize : this.layerSize[l - 1]][];

                for (int i = 0; i < ((l == 0) ? inputSize : this.layerSize[l - 1]); i++)
                {
                    this.weight[l][i] = new double[layerSize[l]];
                    this.previousWeightDelta[l][i] = new double[layerSize[l]];
                }
            }

            // Initialize weights
            // For each layer
            for (int l = 0; l < this.layerCount; l++)
            {
                // Go through each neuron in the current layer
                for (int j = 0; j < this.layerSize[l]; j++)
                {
                    this.bias[l][j] = Gaussian.GetRandomGaussian();
                    this.previousBiasDelta[l][j] = 0.0;
                    this.layerOutput[l][j] = 0.0;
                    this.layerInput[l][j] = 0.0;
                }

                // For each neuron in the previous layer
                for (int i = 0; i < ((l == 0) ? this.inputSize : this.layerSize[l - 1]); i++)
                {
                    // Go through each neuron in the current layer and set the connective weights
                    // to something randomly distributed
                    for (int j = 0; j < layerSize[l]; j++)
                    {
                        this.weight[l][i][j] = Gaussian.GetRandomGaussian();
                        this.previousWeightDelta[l][i][j] = 0.0;
                    }
                }
            }

        }

        #endregion

        #region Methods

        public void Run(ref double[] input, out double[] output)
        {
            // Check if there is enough data
            if (input.Length != this.inputSize)
            {
                throw new ArgumentException("Input data is not of the correcft dimension.");
            }

            // Dimension the output array
            output = new double[this.layerSize[this.layerCount - 1]];

            // Run the network
            for (int currentLayer = 0; currentLayer < this.layerCount; currentLayer++)
            {
                for (int currentNeuron = 0; currentNeuron < this.layerSize[currentLayer]; currentNeuron++)
                {
                    double sum = 0.0;

                    // Compute the sum of the products of the weights (connections) from all neurons
                    // in the previous layer to the current neuron in the current layer
                    // times the output from all nodes in the previous layer
                    for (int prevLayerCurrentNeuron = 0; 
                        prevLayerCurrentNeuron < ((currentLayer == 0) ? inputSize : this.layerSize[currentLayer - 1]);
                        prevLayerCurrentNeuron++)
                    {
                        sum += this.weight[currentLayer][prevLayerCurrentNeuron][currentNeuron] 
                            * ((currentLayer == 0) ? input[prevLayerCurrentNeuron] : this.layerOutput[currentLayer - 1][prevLayerCurrentNeuron]);
                    }

                    sum += this.bias[currentLayer][currentNeuron];

                    this.layerInput[currentLayer][currentNeuron] = sum;
                    this.layerOutput[currentLayer][currentNeuron] = TransferFunction.GetSigmoid(this.transferFunction[currentLayer], sum);
                }
            }

            // Copy the output to the output array
            for (int i = 0; i < this.layerSize[this.layerCount - 1]; i++)
            {
                output[i] = this.layerOutput[layerCount - 1][i];
            }
        }

        #endregion
    }

    public static class Gaussian
    {
        private static Random numberGenerator = new Random();

        public static double GetRandomGaussian()
        {
            return GetRandomGaussian(0.0, 1.0);
        }

        public static double GetRandomGaussian(double mean, double stddev)
        {
            double rVal1;
            double rVal2;

            GetRandomGaussian(mean, stddev, out rVal1, out rVal2);

            return rVal1;
        }

        public static void GetRandomGaussian(double mean, double stddev, out double val1, out double val2)
        {
            double u = 2 * numberGenerator.NextDouble() - 1;
            double v = 2 * numberGenerator.NextDouble() - 1;
            double s;
            double t;

            do
            {
                u = 2 * numberGenerator.NextDouble() - 1;
                v = 2 * numberGenerator.NextDouble() - 1;
            } while (u * u + v * v > 1 || (u == 0 || v == 0));

            s = u * u + v * v;
            t = Math.Sqrt((-2.0 * Math.Log(s) / s));

            val1 = stddev * u * t + mean;
            val2 = stddev * v * t + mean;
        }
    }
}
