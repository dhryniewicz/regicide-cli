# Regicide

A CLI-based clone of the cooperative card game [Regicide](https://www.badgersfrommars.com/regicide), built in Clojure.

## Requirements

- Java 17+
- [Clojure CLI](https://clojure.org/guides/install_clojure) 1.11+

## Running

```bash
# Start a solo game
clj -M:run

# Start with 2-4 players (pass player count as argument)
clj -M:run 2

# Run tests
clj -M:test
```

## How to Play

You are fighting through a castle of 12 enemies — 4 Jacks, 4 Queens, and 4 Kings — using a standard deck of playing cards. Defeat all 12 to win.

### Enemy Stats

| Enemy | Health | Attack |
|-------|--------|--------|
| Jack  | 20     | 10     |
| Queen | 30     | 15     |
| King  | 40     | 20     |

### Turn Structure

1. **Play cards** — select cards from your hand using arrow keys, then press Enter
2. **Suffer damage** — if the enemy survives, select cards to discard to absorb the damage

If you can't discard enough to absorb the attack, you lose.

### Combos

You can play multiple cards together if they share the same rank, as long as the total value is **10 or less**:

- Pair of 5s = 10 damage (valid)
- Pair of 6s = 12 damage (invalid, exceeds 10)
- Triple of 3s = 9 damage (valid)

**Aces** can be paired with any card(s) of the same rank (e.g., Ace + 8 = 9 damage).

### Suit Powers

Each suit has a special power that activates when you play a card of that suit:

| Suit | Power |
|------|-------|
| ♠ Spades   | Reduce enemy's attack by the card's value |
| ♥ Hearts   | Shuffle cards from the discard pile back into the draw pile |
| ♦ Diamonds | Draw cards from the draw pile |
| ♣ Clubs    | Deal **double** damage |

**Immunity**: Enemies are immune to the suit power matching their own suit. A Jack of Spades ignores the Spade power.

### Exact Kill

If your damage equals the enemy's remaining health exactly, the defeated enemy card goes into your hand instead of the discard pile.

### Controls

All controls are instant (no Enter needed except to confirm selection):

| Key           | Action                                          |
|---------------|-------------------------------------------------|
| `←` `→`       | Move cursor between cards                       |
| `↑`           | Toggle selection on current card                |
| `Enter`       | Play/discard selected cards                     |
| `p`           | Toggle hand sorting (unsorted / by suit / by rank) |
| `h`           | Show help                                       |
| `q`           | Quit                                            |
