# Practical Haskell

Welcome to the Github repository for Practical Haskell! As a reminder, lectures, slides, screen-casts can all be found on the [Teachable page](https://academy.mondaymorninghaskell.com/p/practical-haskell) for this course!

This repository contains all the code materials you need for exercises. Each module of the course has a specific branch on the repo. For instance, you'll want to be on the `module-1` branch for databases, `module-2` for servers, and so on.

The exercise PDFs are in the `exercises` directory on each module, as well as the first lecture of the module on Teachable. They'll provide you with instructions for what to do!

## Testing Code

As noted in each exercise handout, certain lectures have tests so you'll know if you've implemented the code properly! The test suite always corresponds to the lecture number (`lecture-2-tests`, `lecture-3-tests`, etc.).

The basic way to run a test suite is with the `stack build` command. For instance:

```
stack build PracticalHaskell:test:lecture-2-tests
```

However, we've provided an easier way. While on the `module-1` branch, run the `stack install` command. This will create aliases for each possible test command on your local path. So instead of the above, you would now be able to just run:

```
test-2
```

## For Help

The best way to reach me for help is through the Slack group or by email at `james@mondaymorninghaskell.me`. For a non-urgent issue, feel free to open an issue or pull request on this repository!
