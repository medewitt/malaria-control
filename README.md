# Malaria Control Strategy Lab

This repository contains a public health undergraduate lecture and simulation lab on malaria control.

## Structure

- `index.qmd`: The landing page and Simulation Lab (built with Shinylive).
- `app/`: Contains the Shiny application source code.
- `presentation/`: Contains the Quarto RevealJS presentation.
- `_quarto.yml`: Quarto website configuration.

## How it works

The website is built using **Quarto** and **Shinylive**. Shinylive allows the Shiny app to run entirely in the web browser using WebAssembly (via webR), eliminating the need for a persistent R server.

The simulation uses the `malariasimulation` package from Imperial College London.

## Deployment

The website is automatically built and deployed to GitHub Pages via GitHub Actions whenever changes are pushed to the `main` branch.

## Local Development

To preview the website locally:

1. Install Quarto.
2. Install the Shinylive extension: `quarto add quarto-ext/shinylive`.
3. Run `quarto preview`.


## Other resoources

https://www.cdc.gov/malaria/php/public-health-strategy/index.html