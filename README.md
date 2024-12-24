# **Analysis of Italian Movies' Success in 2022 and 2023**

This project presents an analysis of Italian films produced in 2022 and 2023. The goal of this analysis was to determine which factors can predict the success of a movie. To achieve this, we curated a comprehensive dataset that includes both conventional movie attributes, such as director, cast, and genre, as well as additional novel parameters that may influence movie success. These parameters include actor popularity on social media, search volumes on platforms like Wikipedia and YouTube, temporal and geographic contexts, and the thematic elements intrinsic to each film's narrative.

## **Objective**

The aim was to predict and evaluate movie success based on two metrics:
1. **Average Rating**: Transformed into a binary value (1 for positive ratings >3/5 stars, 0 for negative ratings <=3/5 stars) to classify audience satisfaction.
2. **Revenue**: Quantifying the box-office performance to gauge the movie’s ability to capture audience interest.

The analysis was performed using binary classification models to predict movie ratings and regression models to predict revenue.

## **Dataset Overview**

The dataset, created by us, includes information on various attributes of Italian movies, such as:

- **Cast**: The actors involved in the film.
- **Genre**: The genre(s) the film belongs to.
- **Production**: The production companies behind the movie.
- **Themes**: Different thematic elements present in the narrative (e.g., Music, Secrets, War).
- **Awards and Nominations**: Information about film nominations and awards (e.g., Cannes, Nastro d'argento).
- **Place Setting**: The geographical context of the film (e.g., Lombardia, Roma, Bari).
- **Time Setting**: Temporal context of the film (e.g., contemporary, historical).

## **Methods Used**

We employed several machine learning techniques to analyze and predict movie success:

1. **Elastic Net (Generalized Linear Model)**: This regularized regression model helped identify the most relevant features influencing movie success.
2. **Support Vector Machines (SVM) with Lasso Penalty**: Used for classification tasks and feature selection, identifying the most important variables for predicting ratings.
3. **Group Lasso**: A method for variable selection that grouped variables based on categories like genre and time setting, assessing which factors had the greatest impact on movie success.

### **Key Findings:**

1. **Elastic Net Model**:
   - Revealed significant coefficients for movie attributes such as cast members, genre, production company, and thematic elements.
   - The presence of the 'Nastro d'argento' award correlated with positive audience ratings.

2. **SVM with Lasso Penalty**:
   - Found that the *Commedia* genre, certain actors (e.g., Miriam Leone), and specific productions (e.g., Warner Bros Italia) were associated with negative ratings (class 0).
   - The Genre "Commedia" was strongly correlated with negative ratings.

3. **Group Lasso**:
   - Identified significant variables within the groups of *Genre*, *Time Setting*, *Nomination*, and *Suggested Audience*.
   - Similar correlations were found when comparing this model with the Elastic Net results.

### **Performance Insights**:

- The models performed well in classifying movies based on audience ratings.
- Regression models were effective in identifying features that significantly impacted box-office revenue.

## **Conclusions**

This analysis highlights the importance of specific movie attributes—such as cast, genre, thematic elements, and awards—in predicting a movie's success. The inclusion of curated variables proved essential in improving the accuracy of success predictions.

However, the study also encountered limitations due to missing data, especially concerning movie budgets, which could have improved the revenue analysis. Moreover, challenges arose with outliers in the dataset, such as the movie *C’è ancora domani*, which had a significant impact on model performance.

In conclusion, this project demonstrates the importance of selecting relevant features for predicting movie success, offering insights into the preferences of Italian moviegoers and the factors that drive a film's success.

## **Project Files**

- **movie_success_analysis.py**: Main analysis script for predicting movie success.
- **data_preprocessing.py**: Data cleaning and preprocessing steps.
- **feature_selection.py**: Methods for feature selection, including Elastic Net, SVM with Lasso, and Group Lasso.

## **Authors**
- Bazzan Sofia
- Cerbero Anna
- Marras Erica
