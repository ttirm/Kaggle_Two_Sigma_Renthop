# 164 Solution of Kaggle competition - Two Sigma Connect: Rental Listing Inquiries

This repository contains the code used in Kaggle - Two Sigma Connect: Rental Listing Inquiries.
To use it, please download the data from Kaggle page and run the scripts in the following order:

- renthop_clean.R
- important_features.R

- renthop_feat_engineering.R

The 3 scripts displayed above are responsible for the data cleaning and feature creation.
From here it is possible to run

- renthop_single_model.R

which applies a single Xgboost model to some of the most important features.
To get the score achieved in the competition (Private Leaderboard: 0.50926) it's necessary to run the following scripts, which generate 11 base models and stack them in a level 1 Xgboost. The level 2 is just a mean of level 1 predictions.

- stack_generate_models.R
- stack_model_level_1.R
- stack_model_level_2.R
