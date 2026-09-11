# Predicting 2025 NHL Free Agent Contracts

A machine learning project developed to predict contract outcomes for
2025 NHL free agents using historical contract data, traditional statistics,
advanced metrics, and player characteristics.

## Approach

Two models were developed:

1. **Contract Term Model**
   Random Forest classification predicting:
   Short (1–2 years), Medium (3–4 years), or Long (5+ years)

2. **AAV Model**
   Cubist regression predicting a player's contract value as a percentage
   of the NHL salary cap.

Predicted contract term was incorporated into the AAV model to generate
complete contract projections.

## Data

Historical free-agent data from 2019–2024 was combined with up to three
seasons of player performance leading into each free-agent year.

Sources included PuckPedia, Spotrac, Hockey Reference, and MoneyPuck.

## Model Performance

| Model | Metric | Result |
| --- | --- | ---: |
| Contract Term | Accuracy | 84.9% |
| Contract Term | Kappa | 0.396 |
| AAV | R² | 0.737 |
| AAV | RMSE | 0.01198 |
| AAV | MAE | 0.0049 |

## Tools

R • Ranger • Cubist • caret • tidyverse


> Note: The original data collection and preprocessing workflow is not
> fully preserved. The available code contains the final modeling and
> prediction stages of the project.
