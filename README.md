# Balatro game in Haskell

This program was created as part of the coursework for the Functional Programming module that I took at the university. The program is written in Haskell programming language.

## Table of contents
- [Overview](#overview)
- [Rules of the game and limitations](<Rules of the game and limitations>)
- [Features](#features)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Usage](#usage)



## Overview
When undertaking the Functional Programming module, we completed two courseworks. This is the 1st one. All the UI was abstracted away from us and we only had to implement the most interesting part, that is, the core logic of the Balatro game, so that a user could play the game themselves and also an AI that can play the game themselves. All the code written by me is located in ``./src/CourseworkOne.hs`` file. The rest of the code was provided from the beginning by the module organiser. 



## Rules of the game and limitations
Note to people who already know of Balatro: This coursework is a
significantly simplified version which only considers Ante 1, Round 1,
with an unmodified deck. Almost every other feature of the game is out
of scope for the coursework.



## Features 
- Playing the game of Balatro (simplified version).
- Letting the AI play the game for you.
- The UI for both playing the game and for watching the AI play the game.



## Prerequisites
- The GHC
- cabal-install
- Stack   

See [this](https://www.haskell.org/downloads/) website for further details.

## Installation
Use ``git clone`` or download the .zip file.

## Usage 
Navigate to the installed folder and run ``stack run`` command to play the Balatro game yourself or run ``stack run ai`` in order to observe how the AI plays the game for you.


