# Evaluating AI Models for Food and Alcohol Ad Classification Against Human Raters

This repository contains the data, complete codebase and analysis pipeline for a comprehensive validation study examining the agreement between AI models (GPT-4o, Gemma, Pixtral, Qwen), crowdsourced humans and domain experts (dieticians) in classifying food advertisements according to WHO guidelines.


## 📁 Repository Structure

```
AI-validation/
├── agreement_calculation/                   # Agreement analysis
│   ├── agreement_functions.R                # Core agreement metrics (Gwet's AC, Jaccard, etc.)
│   ├── consensus_sensitivity.R              # Sensitivity analysis of consensus rules (multi-option questions)
│   ├── human_ai_agreement.R                 # Main analysis script
│   ├── dieticians.R                         # Expert-specific analysis
│   ├── merging_outputs.R                    # Various data processing functions (merging, consensus, etc.)
│   ├── monte_regr.R                         # Monte Carlo simulation functions
│   ├── simulation_multilabel_agreement.R    # Simulation for interpreting multi-label agreement
│   └── outdoor.R                            # Outdoor ad analysis
│
├── data/                                    # Core datasets and model outputs
│   ├── ...                                  # Final datasets with all classifications (crowd, dieticians, models)
│   ├── 1000 images/                         # Main image dataset
│   └── outdoor 100 ads/                     # Outdoor advertising subset
│
├── labeling_validation/                     # AI labeling system
│   ├── AI_labelling_new.py                  # Main labeling script
│   ├── classification.py                    # Classification utilities
│   ├── deduplication.py                     # Data deduplication
│   ├── language_flagging.py                 # Language detection
|   └── WHO_questions.py                     # Question definitions and parsing
│
├── plots/                                   # Generated visualizations and results (tables)
├── publication/                             # Publication materials
│   └── Supplementary Material.docx          # Supplementary documentation
│
├── .gitignore                               # Git ignore rules
├── requirements.txt                         # Python dependencies
├── install2.sh                              # Installation script for the GPU cluster (not necessary)
└── README.md                                # This file


```

## 🚀 Getting Started

### Prerequisites

- **Python 3.8+** with packages listed in `requirements.txt`
- **R 4.0+** with required packages (see agreement scripts)
- **OpenAI API access** (for GPT-4o)
- **Local model setup** (for Gemma, Pixtral, Qwen via `Transformers`)

### Installation

1. **Clone the repository:**
   ```bash
   git clone https://github.com/Paularossi/AI-validation.git
   cd AI-validation
   ```

2. **Set up a virtual environment (venv)**

    Create the virtual environment with:
    ```bash
   python -m venv venv
   ```

   And then activate it with:
   ```bash
   venv\Scripts\activate.bat
   ```

3. **Install Python dependencies (change your CUDA version):**
   ```bash
   pip install "torch==2.10.0" "torchvision==0.25.0" --index-url https://download.pytorch.org/whl/cu128
   pip install -r requirements.txt
   ```

4. **Configure API keys:**
   - Create a `keys.txt` file
   - Add your OpenAI API, HuggingFace and MistralAPI keys
   - Configure local model endpoints


## 📊 Key Features

### AI Labeling Pipeline

- **Multi-model Support**: GPT-4o and Pixtral via the API, Gemma and Qwen via HuggingFace
- **Structured Prompting**: WHO-compliant question formatting
- **Response Parsing**: Automated extraction of classifications
- **Batch Processing**: Efficient handling of large image datasets

### Agreement Metrics

- **Single-option Questions**: Gwet's AC1, Percentage Agreement
- **Multi-option Questions**: Jaccard Similarity, Krippendorff's Alpha (MASI)
- **Statistical Testing**: Z-tests for bias detection + FE regressions
- **Bootstrap Analysis**: Monte Carlo simulations for confidence intervals
- **Consensus Sensitivity**: Sensitivity analysis of alternative consensus rules (baseline thresholded union, empty-threshold, hard intersection, and full union)
- **Multi-option Agreement Simulation**: Contextualization of agreement interpretation for multi-option questions

## 📈 Analysis Outputs

### Generated Visualizations (in `plots/`)

1. **Agreement Heatmaps**: Inter-rater agreement across all question types
2. **Bias Analysis**: Label-level bias patterns by model
3. **Language Comparisons**: Agreement differences between Dutch/French ads
4. **Distribution Plots**: Bootstrap confidence intervals for agreement metrics
5. **Consensus Rule Deltas**: Kripp. Alpha shifts when switching consensus aggregation rules (multi-option)

### Key Results Files (in `data/`)

- Model outputs: `*_all_1000.xlsx` files for each AI model
- Human responses: `responses_human_final.xlsx`, `dieticians_all_final.xlsx`
- Outdoor analysis: `gpt_all_outdoor.xlsx`, `dieticians_outdoor_all_final.xlsx`
- Agreement analyses: Generated during R script execution
- Label mappings: `label_mappings.csv` with all category definitions
- Multi-label simulation results: `multi_agreement_simulation_results.xlsx` (inside `plots`)

## 🏗️ Methodology

### Data Collection
- **Image Dataset**: 1,000+ food/beverage advertisements from Belgium
- **Human Validation**: Dieticians and crowdsourced human coders
- **Multiple Languages**: Dutch and French advertisement content
- **Ground Truth**: Consensus-based gold standard labels

### AI Model Configuration
- **API**: GPT-4o and Pixtral with structured prompts
- **Open Models**: Gemma and Qwen via deployment on the Maastricht University's GPU clusters through HuggingFace
- **Consistent Prompting**: Standardized WHO question format across all models
- **Response Validation**: Automated parsing and error checking

### Agreement Analysis
- **Multiple Metrics**: Appropriate for different question types (single-choice vs multi-label)
- **Statistical Rigor**: Confidence intervals and significance testing
- **Bias Quantification**: Z-tests for systematic differences between AI and human responses
- **Robustness Checks**: Bootstrap simulations and sensitivity analysis

## 🔬 Research Applications

This codebase enables research in:

- **Public Health**: Automated food marketing surveillance and policy compliance
- **AI Reliability**: Model validation for domain-specific classification tasks
- **Content Analysis**: Systematic advertisement classification at scale
- **Regulatory Compliance**: WHO guideline adherence monitoring

## 📋 Question Categories

### Single-Option Questions
- **Alcohol Presence**: Binary alcohol detection
- **Target Group**: Child/Adolescent/Adult targeting
- **Ad Type**: Business classification (manufacturer/retailer/restaurant, etc.)

### Multi-Option Questions  
- **WHO Food Categories**: 25 standardized food classifications
- **Marketing Strategies**: 11 marketing techniques
- **Premium Offers**: 10 promotional elements and incentives

## 📄 Citation

If you use this code or methodology in your research, please cite accordingly.

## ⚖️ License

This project is developed for academic research at Maastricht University. Please contact the authors for usage permissions and collaboration opportunities.
