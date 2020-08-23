# HedgeCaml
HedgeCaml is a Stock Market Game that I built with Greg George and Justin Kobler for CS3110 @ Cornell. Download the repo and type "make play" in the hedgecaml directory to play!

When we first set out to build a stock market game, our group planned on implementing a simple environment that would emulate the fine unbalances and nuances of a stock market. We wanted to make a game where the player could know what to expect but must always be ready for the risks and rewards that come with the uncertain nuances that make real global markets such a challenge to predict. Implementing this remains our main goal for the project, everything else simply existing to make the experience more immersive and enjoyable. To this degree, our first sprint implemented the trend based model we decided to use when writing the project charter. However, after talking to some friends who are familiar with economics and markets, we have listened to and considered new additions that could be implemented to make the model even more realistic. One of these features will be tying together the trends of stocks in related industries, be they positively or negatively correlated. Therefore, as can be seen in the real world, industries that are doing well will see the companies it is comprised of thriving and vice versa, allowing the player the potential for profit if they notice the opportunity in time.

We ultimately implemented the following commands and features in our stock market game.

- **History [Company Name] -** shows stock prices from past 10 rounds of the specified company
    - We implemented a **Price [Company Name]** that displays the current share price and stock prices from past 10 rounds (from oldest to most recent).
- **Owned** - show a list of owned stocks and their current prices
    - We implemented **Portfolio** which lists purchased stocks with their current share prices and the amount of shares owned.
- **Win State**
    - Players must make a certain amount of money (depends on difficulty) in 100 rounds to win.
- **Lose State**
    - Players lose if they make less than a certain amount of money (depends on difficulty) in 100 rounds.
- **Sandbox Mode**
    - We implemented a mode without win or lose states, that allows players to play infinitely to try to make the most profit.
- **Difficulties**
    - We implemented easy, medium, and hard difficulties which vary the amount of money that players must make in 100 rounds to win.
- S**ave and Load**
    - We implemented save and load functionalities that allow players to save their games to json, quit, and resume the same game.
- **Graphical Representation of Price Changes**
    - We implemented text-based graphs to better represent changes in share prices for individual stocks.

If we were to continue working on HedgeCaml in the future, we would likely implement the following:

- Switch from a text-based interface to a graphics-based interface using third-party libraries.
- Implement order history functions that allow players to view all of their past buy and sell orders.
- Add options to short sell stocks (betting that a stock’s price will decrease), and make stop, stop limit, and limit orders (set lower and upper bounds for when to execute buy and sell orders).
- Use real data from APIs like Alpha Vantage.
- Implement a scoreboard based on average earnings per round to let players compete against each other or themselves.
- And more!

All in all, we very much enjoyed building HedgeCaml and we learned a lot (about OCaml, modules, etc.) in the process!
