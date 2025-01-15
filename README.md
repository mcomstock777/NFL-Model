Short version (How to):

If you don't have time to look through all the bells and whistles of the app please visit tab 6 (Prediction O/U Aggregated Models) and tab 9 (Prediction Spread Aggregated Models). Picks in our wheelhouse will be highlighted green.
Additional information in the View Instructions tab below.
The appendix will have additional model info, picks I agree/disagree with the model on for O/U and spread, player prop locks of the week and what's trending up/trending down in the NFL.

Long version (How to):

The only tabs you really need to use are the Prediction O/U Aggregated Models (tab 6) and Prediction Spread Aggregated Models (tab 9). This is where you go if you want to spend the least time in this app and just get the picks as noted above.
All picks that have a yes on the 'Mod All Agree High Conf' are in our wheelhouse (Should be highlighted green) and most of these are picks we will be choosing (We don't use all picks). 
You can use the up and down arrows on each column in the table to sort things how you would like and there are filters if you would like to get certain cuts of data.

The lines we are pulling into the model are based on Draftkings, so if you use a different app/see a different line just keep that in mind that you may need to use an alternate line to matchup with when we ran the model. Also, lines do move quite often throughout the week and they are pretty volatile within 24 hours of the game.

The yes in column `Mod All Agree High Conf` are picks where all 3 models agree on an outcome and they have high enough confidence. The `High Conf` section is just where, in aggregate, the models average to high enough confidence, but at least one doesn't agree on an outcome. `Mod All Agree` is where all 3 models agree, but the confidence isn't as high as we'd like. 

For under's and away team covering we have the high conf threshold at 59.5%+, so you should notice those being highlighted green as long as all 3 models also agree. For over's and home team cover we lowered the threshold to 58%+ for home team covers and 57% for over's because of how the model leans towards unders and away team cover. I found that lowering that confidence threshold for home team and over allows us to play more games and actually improves accuracy overall.

To highlight some of the alpha we found in 'Mod All Agree High Conf', take a look at tab 4 (Hist OU Agg Models) or tab 7 (Hist Spread Agg Models) and hit update. 
You'll notice the regular win % is quite a bit lower than the win % for 'Mod All Agree High Conf'. Pretty staggering difference, isn't it? 
I found this little sweet spot before the 2023 season and it lead to a 60%+ overall accuracy for the model in it's rookie year. Hedge funds would kill for that!

If you want information on the tiers, injuries, bet types, etc, please look in the appendix for more information. We tend to use the round robin betting style for our bets. 
The explanation of how round robins work can be seen in the 'Round Robin Calculator' tab and the appendix.

About 3 Algorithms Used:

XGBoost: Best performing individual model for spread. XGBoost is like having a group of experts who learn from each other's mistakes to get better and better. 
Imagine a classroom where a teacher gives a test, and every time a student makes a mistake, the teacher gives extra help on that specific problem. 
XGBoost builds a series of decision trees, where each new tree focuses on the mistakes made by the previous ones. By doing this, it creates a very strong model that makes highly accurate predictions.

Random Forest: Not the best performer at either spread or over/under but nonetheless has good enough accuracy to be included. Random Forest is like asking a group of experts for their opinions to make a decision. 
Each expert is given different information and comes up with their own conclusion. Then, they all vote, and the majority vote is the final decision. 
In Random Forest, each expert is actually a decision tree, which makes decisions by asking a series of yes/no questions.

Logistic Regression: Best performing individual model for over/under. Logistic regression is like a smart way of drawing a line that separates two groups. Imagine you have a bunch of marbles—some are red, and some are blue. 
Logistic regression tries to divide the space so that most of the red marbles end up on one side and most of the blue marbles on the other.
