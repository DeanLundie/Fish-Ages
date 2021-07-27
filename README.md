# **Modelling the Age of Fish Using the EM-Algorithm**

<p align="center">
  <img src="https://user-images.githubusercontent.com/86531906/127075226-56616c10-c44a-459d-b9ee-2a28c6fb4748.png" />
</p>

## **Summary**
The length of fish and the age of fish are closely correlated. The dataset that will be focused on in this project includes 1000 observations of fish lengths. For 100 of these observations, the age of the fish has been determined scientifically by measuring the number of growth layers on the otolith (a small ear bone). However, for the remaining 900 fishes, only the length is known. The aim of this project is to therefore estimate the corresponding age of these fish based only on their length. As can be seen in the graph below, the lengths of the fish seem to follow a Gaussian Mixture Model with three component distributions. Rather than simply using an expectation-maximisation (EM) algorithm created in an external package, the EM-algorithm will be created from scratch

<p align="center">
  <img src="https://github.com/DeanLundie/Fish-Ages/blob/main/MixtureModel.png" />
</p>

## **Expectation-Maximisation (EM) Algorithm**
When an independent mixture model is needed to model the distribution of a given random variable, the EM algorithm can be used to obtain parameter estimates for each independent mixture. In this case, considering that the age structure of the fish follows a Gaussian mixture model with three components, we can use the EM algorithm to obtain the mean and variance for each of the three normal distributions. 

To find out more about the theory of the EM algorithm checkout [Do, C.B. and S. Batzoglou. 2008](https://www.nature.com/articles/nbt1406). Generally, there are four main steps in the EM algorithm:

* **1\. Initialisation** :  To begin the algorithm, the k-means clustering algorithm was used to split the observations into three groups (young, adult or old) and to calculate initial values for group means, variances and probabilities.
* **2\. Expectation** :  Using Bayes Rule and the current best estimates of the group means and standard deviations, the posterior probability a length observation belonged to each group was calculated.
* **3\. Maximisation** :  Using a weighted mean formula and the probability of an unknown length observation belonging to each age group, the best estimates of the group means and probabilities were updated. Using this new estimate for the group means, the group standard deviations were also updated, giving the new best estimates for the means, standard deviations and probabilities.
* **4\. Convergence Testing** :  To test if convergence to the best grouping and parameter estimates has occurred, the log-likelihood was computed at each iteration and compared to the log-likelihood of the previous iteration. If at any iteration the difference between the log-likelihoods are above a specified threshold (tolerance level), the expectation and maximisation steps are repeated to obtain new best estimates and the convergence step is then applied to these new values. This process is repeated until the difference in log-likelihoods is below the threshold value. Once the difference between these two values is below a specified threshold (tolerance level), convergence has occurred, and the algorithm ends. The optimal groupings and estimates for the means, standard deviations and probabilities have been achieved.

These steps can be visualised in the diagram below

<p align="center">
  <img src="https://github.com/DeanLundie/Fish-Ages/blob/main/Flowchart%20-%20Implementation.jpg" />
</p>
