# Lorys-Project

For the @Emurgo Developer Associate Program
This project aims to demonstrate a sufficient proficiency in Haskell to be able to learn and work with Plutus & Cardano Smart Contracts.

## Introduction

This project is an attempt of having a genetical algorithm simulation in Haskell. It do not solve any real problem but demonstrate how to approach such project in Haskell.
The Haddock documentation is available here: [Haddock Description of Lorys's Project](https://loryshamadache.github.io/Haskell-Project/)
In this documentation you will be able to find all the classes & function and their description

## Simulation

I am simulating the behavior of "Characters" in an environment with food. These Characters wander randomely in the bounded environment. These Characters have a speed, a field of vision, a size ... etc. When they encounter food, they change direction to get it. Food gives them energy that they lose through time. In this environment, next generation of characters adjust their speed for better survival.

## General Process

I am using the Gloss Library for vizualization and classical genetical algorithm for the process

![Process](project_diagram.png)
