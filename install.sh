#!/bin/bash
# to run this script, first make it executable (only once): `chmod +x ./persistent/AI_validation/install.sh`
# then run it with `./persistent/AI_validation/install.sh`

# this file is structured as follows:
# 1. set up python and conda path variables
# 2. download conda if needed, create and activate the environment
# 3. download all relevant python packages
# 4. download ollama if needed, start ollama 
# 5. pull ollama models

echo "📂 Setting up GPU workspace..."

# Define Conda installation directory and environment name
CONDA_DIR="/opt/conda"
ENV_NAME="my_project"

export PATH="${CONDA_DIR}/bin:$PATH" # Ensure Conda is in PATH for installation

# Prevent duplicate PATH entries
if [[ ":$PATH:" != *":/opt/conda/bin:"* ]]; then
    echo 'export PATH="/opt/conda/bin:$PATH"' >> ~/.bashrc
fi


# new approach to install conda forge
# if [[ ! -z "${CONDA_DIR}" && ! -d "${CONDA_DIR}" ]] ; then
#     echo "Conda not installed, installing it."

#     echo "export CONDA_DIR=$CONDA_DIR" >> ~/.zshrc
#     echo "export CONDA_DIR=$CONDA_DIR" >> ~/.bashrc

#     # Automatically download the latest release of mambaforge for conda/mamba
#     wget -O Mambaforge-Linux-x86_64.sh https://github.com/conda-forge/miniforge/releases/latest/download/Mambaforge-Linux-x86_64.sh
#     chmod +x Mambaforge-Linux-x86_64.sh
#     /bin/bash Mambaforge-Linux-x86_64.sh -f -b -p "${CONDA_DIR}"
#     rm "Mambaforge-Linux-x86_64.sh"

#     mamba config --system --set auto_update_conda false
#     mamba config --system --set show_channel_urls true

#     # Install Tensorflow
#     mamba install -y tensorflow tensorboard
#     pip install jupyter_tensorboard

# else
#     echo "Conda already installed."
#     if ! command -v mamba &> /dev/null
#     then
#         echo "Mamba not installed. Installing it."
#         conda install -y -c conda-forge mamba
#     fi
# fi


# Check if Conda is already installed
if [ -d "$CONDA_DIR" ]; then
    echo "✅ Conda is already installed at $CONDA_DIR"
else
    echo "🔽 Downloading and installing Miniforge..."
    
    # Fetch latest Miniforge download URL from GitHub API
    DOWNLOAD_URL=$(curl -s https://api.github.com/repos/conda-forge/miniforge/releases/latest | grep browser_download_url | grep -P "Miniforge3-\d+((\.|-)\d+)*-Linux-x86_64.sh" | grep -v sha256 | cut -d '"' -f 4)

    echo "Downloading latest Miniforge from: $DOWNLOAD_URL"
    
    # Download and install Miniforge
    curl -Lf -o miniforge.sh "$DOWNLOAD_URL"
    /bin/bash miniforge.sh -f -b -p "${CONDA_DIR}"
    
    # Clean up installer
    rm miniforge.sh

    echo "✅ Miniforge installed successfully at $CONDA_DIR"
fi


# Ensure Conda is initialized properly
eval "$(conda shell.bash hook)"
conda init bash

# Reload shell to apply Conda init changes
source ~/.bashrc

# Create Conda environment if it doesn’t exist
if conda env list | grep -q $ENV_NAME; then
    echo "✅ Conda environment '$ENV_NAME' already exists."
else
    echo "🔧 Creating Conda environment: $ENV_NAME..."
    conda create -y -n "$ENV_NAME" python=3.11
fi

# Activate Conda environment
echo "Activating Conda environment..."
#eval "$(conda shell.bash hook)"
conda activate "$ENV_NAME"

# Install required Python packages
echo "📦 Installing Python packages..."
#conda install -y -c pytorch torchvision transformers openai 
#conda run -n "$ENV_NAME" pip install --upgrade pip
#conda run -n "$ENV_NAME" pip install ollama requests mistralai pillow pandas numpy accelerate

# Verify if Ollama is installed
if ! pgrep -x ollama > /dev/null; then
    echo "🔽 Installing Ollama..."
    curl -fsSL https://ollama.com/install.sh | sh
else
    echo "✅ Ollama is already installed."
fi

# Start Ollama automatically if not already running
if ! pgrep -x ollama > /dev/null; then
    echo "🚀 Starting Ollama in the background..."
    nohup ollama serve > /workspace/ollama.log 2>&1 &
else
    echo "✅ Ollama is already running."
fi

sleep 5  # Give it a few seconds to start

# Pull required Ollama models
echo "🔽 Downloading Ollama models..."
ollama pull llama3.2-vision:11b
ollama pull llava:7b
# ollama pull llava-llama3
# ollama pull bakllava

# Add AI_validation to PYTHONPATH
if ! grep -q "PYTHONPATH=" ~/.bashrc; then
    echo 'export PYTHONPATH="/workspace/persistent/AI_validation"' >> ~/.bashrc
fi
export PYTHONPATH="/workspace/persistent/AI_validation"

# Ensure Conda is in PATH for future sessions
echo 'eval "$(conda shell.bash hook)"' >> ~/.bashrc

sleep 20
#oc patch dc/ai-labeling-gpu --type=json -p='[{"op": "replace", "path": "/spec/template/spec/containers/0/resources", "value": {"requests": {"nvidia.com/gpu": 1}, "limits": {"nvidia.com/gpu": 1}}}]'

conda init
conda activate "$ENV_NAME"

# Check if GPU is available
echo "🔍 Checking GPU availability..."
python3 -c "import torch; print(f'CUDA Available: {torch.cuda.is_available()}')"

echo "✅ Installation complete!"
ollama list 
echo "To activate the environment, run: conda activate $ENV_NAME"

# activate conda and the environment with `conda activate my_project` and check available environments with `conda env list`
# to restart the server run `source ~/.bashrc` and then again `conda activate my_project`
