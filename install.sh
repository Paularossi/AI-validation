#!/bin/bash
# to run this script, first make it executable (only once): `chmod +x install.sh`
# then run it with `./install.sh`

echo "📂 Setting up GPU workspace..."

# Define Conda installation directory and environment name
CONDA_DIR="/opt/conda"
ENV_NAME="my_project"

# add conda to path
export PATH="$CONDA_DIR/bin:$PATH"

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


# Initialize Conda
export PATH="${CONDA_DIR}/bin:$PATH"

# Create Conda environment if it doesn’t exist
if conda env list | grep -q $ENV_NAME; then
    echo "✅ Conda environment '$ENV_NAME' already exists."
else
    echo "🔧 Creating Conda environment: $ENV_NAME..."
    conda create -y -n "$ENV_NAME" python=3.11
fi

# Activate Conda environment
echo "Activating Conda environment..."
conda activate "$ENV_NAME"

# Install required Python packages
echo "📦 Installing Python packages..."
conda install -y -c pytorch torchvision transformers openai 
pip install --upgrade pip
pip install ollama requests mistralai pillow pandas numpy accelerate

# Verify if Ollama is installed
if ! command -v ollama &> /dev/null
then
    echo "🔽 Installing Ollama..."
    curl -fsSL https://ollama.com/install.sh | sh
else
    echo "✅ Ollama is already installed."
fi

# Start Ollama automatically
echo "🚀 Starting Ollama in the background..."
nohup ollama serve > /workspace/ollama.log 2>&1 &

# Add AI_validation to PYTHONPATH
export PYTHONPATH="/workspace/persistent/AI_validation"
# Ensure it persists across sessions
echo 'export PYTHONPATH="/workspace/persistent/AI_validation"' >> ~/.bashrc


# Check if GPU is available
echo "🔍 Checking GPU availability..."
python3 -c "import torch; print(f'CUDA Available: {torch.cuda.is_available()}')"

echo "✅ Installation complete!"
echo "To activate the environment, run: conda activate $ENV_NAME"

# activate conda and the environment with `conda activate my_project` and check available environments with `conda env list`
# to restart the server run `source ~/.bashrc` and then again `conda activate my_project`
