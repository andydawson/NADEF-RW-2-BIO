# NADEF-RW-2-BIO

Forests play an important role in the carbon cycle. However, climate-vegetation dynamics are still not well understood. How will forest carbon sequestration change in response to climate change? Can we use ring-width data to quantify biomass trajectories, with uncertainty, to ultimately improve ecosystem models?

The objective of this project is to estimate aboveground biomass and aboveground biomass increment trajectories with uncertainty using ring-width and census data. We apply this model to a research forest stand in the Abitiby region of Quebec. Within this forest stand, the FERLD established a 1 ha permanent sample plot, where all tree diameters were remeasured several times from 1994 through 2019. For some of these trees, there are additional ring-width time series from tree cores taken in 2014.

Using the census and ring-width data, we implement a Bayesian hierarchichal model to estimate annual growth (increment, and diameter). This framework allows us to account for data and process uncertainty, and estimate annual growth for all trees in the stand, even those for which we do not have ring-width data. The Bayesian framework makes this possible by borrowing information across trees and through time.

We use the Stan statistical language to specify the model and estimate parameters (``tree_model.stan``).

## Workflow

1. Prepare / clean the data: ``read_data_D1823.R``.
2. Format the data for model input: ``build_data_D1823.R``
3. Run the annual growth model: ``run_model_D1823.R``
4. Plot the growth model output: ``plot_model_D1823.R``
5. Get biomass from annual growth model diameters: ``get_biomass_D1823.R``
