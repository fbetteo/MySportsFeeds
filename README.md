# MySportsFeeds

### Important Stuff

Links: 
* https://squared2020.com/2017/09/18/deep-dive-on-regularized-adjusted-plus-minus-i-introductory-example/
* https://squared2020.com/2017/09/18/deep-dive-on-regularized-adjusted-plus-minus-ii-basic-application-to-2017-nba-data-with-r/
* https://squared2020.com/2018/12/24/regularized-adjusted-plus-minus-part-iii-what-had-really-happened-was/
* http://www.sloansportsconference.com/wp-content/uploads/2015/09/joeSillSloanSportsPaperWithLogo.pdf
* http://bjlkeng.github.io/posts/probabilistic-interpretation-of-regularization/
* https://podcasts.apple.com/us/podcast/1-ryan-davis-adjusted-plus-minus-and-friends/id1469516033?i=1000443010829


### Adjusted +-

If the player is added to the court, we must remove another player from the court. So let’s suppose that players A1, A2, and A3 are on the court. Suppose we wish to replace player A3 with player A4; while Team B introduces no changes. The difference between these lineups are (beta_4 – beta_3). To see this, take the proposed lineup and subtract the current lineup. This is what we get on the right hand side. However, on the left hand side, we get Y_2 – Y_1, where 2 just means proposed stint while 1 means current stint. This is the difference in differential per 100 possessions.

This means that the beta values are relative expected differential in contribution of a player with respect to replacement. This is not a player’s pure contribution, but rather their relative contribution. In fact, a player with positive weight can be a negative contribution. How so? Replace a player with an 8.049 beta with a player that has a 2.569 beta. We expect the new player to contribute less on the court than the previous player; as they are realistically a -5.48 contribution with respect to the previous line-up.

### Assumptions

First, we assume that contribution of a player can be realized by their expected value. That is, a player playing in their first minute is assumed to have the same impact as their 43rd minute, regardless of tempo, bench time, or opponent strategy.

Second, we assume that we can solve this system of equations. Sure, we can apply mathematics and get numbers; but they may not be reliable as the methodology may not satisfy our assumption

Finally, we like to identify these estimates to help with prediction. In order to do this, we must assume some sort of distribution on the data. The most commonly selected distribution? The Gaussian distribution. It’s nice and has fantastic properties, but is difficult to satisfy. For instance, it assumes that our data is continuous and takes a nonzero probability over the real line. That’s already violated as differential per 100 possessions is a discrete value that effectively maximizes at 400, barring crazy free-throw situations. Situations as this would be converted basket, missed free throw and offensive rebound, converted basket; etc.

However, most folks are willing to ignore the violation of these assumptions and proceed as usual. That’s not a major problem, provided there is enough data, and they can be indicated as being roughly shaped as Gaussian. We will return to this when proper.

### Least Squares

Multiplying the player matrix by it's transposed we get:

The player matrix gains a really nice property. We now see the number of interactions between the players. For instance Player A1 played in 8 of the 11 stints. Furthermore, Player A1 player with A2 four times, A3 five times, A4 four times, and A5 three times. Similarly, Player A1 played against B1 and B2 each five times, B3 and B5 four times, and against Player B4 six times. This matrix now counts the number of interactions between every player!

By multiplying by the inverse, we now obtain the average traditional (+/-) per 100 possessions per interactions of players on the court. This effectively identifies the amount of contribution that a player yields during 100 possessions, relative to the personnel played against! So let’s solve this.


### Error Estimation
As we can see, the variance is HUGE. Emphasis on huge. So much so that none of the values above allow us to state, statistically speaking, that Player B4 is indeed the best player on the court. So while APM is a huge improvement over the (+/-) statistic, it still fails miserably in its basic sense.


### RIDGE
Regularized Adjusted Plus Minus (RAPM)

If we look at the player interaction matrix above, we find that the rank of such a matrix will always be at most N-1, where N is the number of players. Think of the reasoning this way. We know in an NBA game, there are ten players on the court at any given time. Suppose there are N players total in the league. Each degree of freedom, or independent pieces of information, is equivalent to asking each player “are you in the game right now?” If we ask the first N-1 players, we know whether player N is in the game without having to ask him, regardless of permutations in asking which players.

If this rank is never N, then we cannot invert the player interaction matrix! Hence, another methodology is required. Such a methodology is given by ridge regression. Ridge regression applied to APM yields the model most commonly known as Regularized Adjusted Plus Minus (RAPM).
Ridge Regression

Ridge regression is a Bayesian filter with a particular goal in mind: if we apply a slight perturbation to the player interaction matrix, we can guarantee the matrix is invertible. While we introduce a slight bias in the results, the bias is nearly negligible and we obtain low-variance estimates for each player. And we don’t have to throw out players.

This value of lambda attempts to control multicollinearity between the players.

Ridge regression is actually a Bayesian filtering process, where the coefficients are seen as random quantities that follow a mean-zero Gaussian distribution, called a prior distribution. The value of lambda identifies the variance associated with the Gaussian and therefore controls how quickly each beta value will shrink with respect to lambda.

In light of this, the coefficients are interpreted as follows: amount of differential contribution with respect to variation. Here, the variation is lambda; which filters large-magnitude values of beta out. We can think of this as approximate differential contribution scaled by lambda.

Taking a look at the players from our simple example, we have that Player B4 is the top player; while Player B2 is right beside him. Effectively, this agrees with (+/-), but more importantly is attempting to negate out the effects of Player B1 on both players.

What we cannot state is that Player B4 contributes 1.17724 more points per possession for his team than Player B1 in the same situation. This has to be said, relative to lambda = 20. Regardless, this provides for a methodology of understanding the value of a player within a particular system; and hopefully gives insight on how PM, APM, and RAPM work.

### Parte3

The value XtY is the ADDITIVE RATING across all stints. I emphasized additive ratings as we are adding ratings regardless of the number of possessions. As an example, if a stint has played twice with a rating of 200 and 100. The resulting value in XtY is 150. In truth, the rating is really 109.09 as the two ratings are derived from one stint with 2 points over 1 possession and another stint with 10 points over 10 possessions. As a flaw with RAPM, this is a commonly accepted atrocity whenever RAPM is computed. For this season, it happens A LOT.


Since we are working with a Gaussian distribution, we can compute the test for comparison… we obtain a test-statistic of approximately 0.05; which has a ridiculously high p-value. This indicates the difference between first and fiftieth is not discernible. That’s right… being the top in RAPM is effectively meaningless from a statistical stand-point. And that’s the rub; RAPM is not an effective tool to significantly measure the impact of a player. It’s just a tool to rank guys and hope no one notices all the pitfalls along the way.
