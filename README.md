# Cat vs Dog Image Classifier

This repository implements a simple neural network in Scala designed to classify images as either a "cat" or "dog". The neural network is trained on a dataset of labeled images and uses various helper utilities for data loading, matrix operations, and other essential functions.

## Files Overview

- `Main.scala`: The main entry point of the program. It orchestrates the training process and tests the network.
- `dataloader.scala`: A module responsible for loading and preprocessing image data into a format suitable for the neural network.
- `matrix.scala`: Defines matrix operations necessary for the neural network, such as addition, multiplication, and activation functions.
- `neuralnetwork.scala`: Contains the core implementation of the neural network, including the forward and backward propagation algorithms.
- `utils.scala`: Utility functions for various tasks, such as data manipulation and performance metrics.

## Requirements

To run this project, you'll need the following:

- Scala 2.13+ (can be installed via [SDKMAN](https://sdkman.io/) or [Homebrew](https://brew.sh/))
- [sbt](https://www.scala-sbt.org/) (Scala Build Tool)
- A dataset of labeled cat and dog images (such as the [Kaggle Cats and Dogs Dataset](https://www.kaggle.com/c/dogs-vs-cats/data))

## Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/axelgunnarsson2004/CatvsDog
   cd CatvsDog
