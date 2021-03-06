# A humble solution to Bonus2

This is a solution to Bonus2 homework of FP course. The solution is far from perfect. Even tough it runs without an error and satisfies all of the requirements given in the assignment, it has many issues. You can see this repository as a baseline solution to the problem. We encourage you to **SEND PULL REQUEST** to improve the quality of code. Also, we suggest you to follow (click Watch) this repository as there will be improvements. Here are some problems you might want to fix:
- `removeCard` function can be simplified
- `GameState` type is not defined.
- `runGame` function can be simplified with new `GameState` type.
- `readCards` and `readMoves` functions can be improved.

You are not bounded only to these problems, feel free to change anywhere else in the code if you see an improvement. You can even rewrite the functions with higher order functions to get rid of annoying recursions. If you are not familiar with concept of *'Sending Pull Request'*, you can look at this [link](https://help.github.com/articles/creating-a-pull-request/)

### Testing

You can use [calico](https://calico.readthedocs.io) to test the program. To run the tests (in main directory):

> calico ./test/solitaire.yaml
