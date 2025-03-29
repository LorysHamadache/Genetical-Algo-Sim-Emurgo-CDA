# Genetic Algorithm Simulation in Haskell  
*Project by Lorys H — for the @Emurgo Developer Associate Program*

This project showcases proficiency in **Haskell**, with the goal of preparing for development on **Plutus** and **Cardano Smart Contracts**.

---

## 🧬 Project Overview

This is a simulation of a **genetic algorithm** implemented in Haskell. While it does not aim to solve a specific real-world problem, it demonstrates how such an algorithmic structure can be designed and visualized using functional programming.

- **Haddock Documentation**:  
  📚 [View API Docs](https://loryshamadache.github.io/Haskell-Project/)  
  Contains all modules, types, and function descriptions.

---

## 🧠 Simulation Concept

The environment simulates autonomous **characters** evolving over time:

- Characters move randomly in a 2D space.
- Each character has properties: speed, field of vision, size, etc.
- Characters consume **food** to gain energy, which is depleted over time.
- Traits like **speed** evolve via genetic inheritance to increase survival chances in future generations.

---

## ⚙️ Technical Details

- **Language**: Haskell  
- **Visualization**: [Gloss](https://hackage.haskell.org/package/gloss) library  
- **Algorithm**: Basic genetic algorithm loop (selection, crossover, mutation)

---

## 🧭 Simulation Flow

![Simulation Process](project_diagram.png)

---

## 📁 Structure

- `Main.hs` — entry point and simulation loop  
- `World.hs` — environment definition  
- `Character.hs` — traits, behavior, and energy model  
- `Genetic.hs` — reproduction and mutation logic  
- `UI.hs` — Gloss integration for display  

---

## 🧪 Run It

```bash
cabal build
cabal run
```

## 🔗 Links

- [Cardano Developer Portal](https://developers.cardano.org/)  
- [Gloss Documentation](https://hackage.haskell.org/package/gloss)  
- [Haddock Documentation](https://loryshamadache.github.io/Haskell-Project/)  

