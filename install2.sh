#!/bin/bash
# create a virtual environment first,
# then make this script executable (only once): `chmod +x install2.sh` and run it with `source install2.sh`

echo "📂 Setting up GPU workspace..."

# Install required Python packages
echo "📦 Installing Python packages..."
pip install --upgrade setuptools pip wheel

# install torch and torchvision separately for version compatibility
pip install "torch==2.10.0" "torchvision==0.25.0" --index-url https://download.pytorch.org/whl/cu128
python -m pip install -r requirements.txt

# Check if GPU is available
echo "🔍 Checking GPU availability..."
python3 -c "import torch; print(f'CUDA Available: {torch.cuda.is_available()}')"

echo "✅ Installation complete!"
