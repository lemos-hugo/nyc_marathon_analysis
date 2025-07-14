# NYC Marathon Performance Analytics

## Overview
This is a project done for the NYC Data Science Academy. This R script analyzes NYC Marathon results to provide insights for training programs and performance optimization - using machine learning to predict finish times and identify market opportunities in the running training industry.

## Dataset
- **Source**: NYC Marathon Results https://www.kaggle.com/datasets/runningwithrock/nyc-marathon-results-all-years?select=NYC+Marathon+Results.csv
- **Size**: ~1M race results (stratified sampling applied)
- **Variables**: Age, Gender, Finish Time, Geographic Location, Overall Rank
- **Time Period**: Multi-year marathon data

## Technologies Used
- **R**: tidyverse, randomForest, caret, ggplot2
- **Machine Learning**: Random Forest, Linear Regression
- **Analysis**: Performance prediction, market segmentation, benchmarking

## Key Features
- **Predictive Modeling**: Random Forest model for finish time prediction (R² = 0.45-0.65)
- **Performance Benchmarking**: Age-group and gender-based percentiles
- **Market Segmentation**: Runner categorization for training services
- **Training Recommendations**: Personalized pace zones by performance level

## Project Structure
```
NYC-Marathon-Analysis/
├── nyc_marathon_analysis.R          # Main analysis script
├── NYC Marathon Results.csv         # Dataset
├── README.md                        # Documentation
└── plots/                           # Visualizations
```

## Key Insights

### Performance Predictors
- **Age**: Primary factor with non-linear decline after 35
- **Gender**: 20-30 minute performance gap across age groups
- **Geographic Origin**: Elite countries show superior performance
- **Peak Ages**: 25-35 for optimal marathon performance

### Market Segmentation
| Category | Time Range | Market Share | Business Opportunity |
|----------|------------|--------------|---------------------|
| Elite | Sub-3:00 | 5% | Premium coaching ($3K-10K annually) |
| Competitive | 3:00-3:30 | 15% | Advanced programs ($1K-3K annually) |
| Good | 3:30-4:00 | 25% | Performance focus ($500-1.5K annually) |
| Average | 4:00-4:30 | 30% | **Largest segment** ($500-1.5K annually) |
| Recreational | 4:30-5:00 | 20% | Beginner programs ($200-800 annually) |
| Survival | 5:00+ | 5% | First-timer support ($200-500 annually) |

### Business Applications
- **Training Companies**: 55% of runners in Average/Recreational segments represent high-volume opportunity
- **Age-Group Specialization**: 25% Masters athletes (50+) willing to pay premium prices
- **Geographic Expansion**: International markets show distinct performance patterns
- **Personalized Coaching**: Data-driven pace recommendations by performance level

## Model Performance
- **Random Forest R²**: 0.45-0.65 (balanced performance without overfitting)
- **RMSE**: 25-35 minutes prediction accuracy
- **Key Features**: Age, Gender, Age-Gender interaction, Geographic factors
- **Validation**: Robust train/test split with overfitting prevention
