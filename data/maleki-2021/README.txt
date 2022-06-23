Variable description for the EASTERN_BOREAL_MIXEDWOODS_CANADA.zip dataset.

For a detailed description of the sampling design and methods, please consult the data publication: Maleki et al. "A 249-year chronosequence of forest plots from eight successive fires in the Eastern Canada boreal mixedwoods" (under review)

The dataset is split into three sub-datasets: LDTRF-TRANSECTS, LDTRF-SEEDLINGS and LDTRF-HECTARES, with each sub-dataset containing multiple tables organized following relational database principles (one table per level of observation, with ID fields linking tables).

---

LDTRF-TRANSECTS

Plots: transect_plots.csv
Variable	Data type	Description
plot_id		String		12-character plot ID formed by transect ID and distance along transect
fire_year	Integer		Year of last fire in stand
transect	String		Transect ID formed by fire year and transect number
long		Real		Longitude of plot (decimal degrees)
lat		Real		Latitude of plot (decimal degrees)
		
Measurements: transect_meas.csv
Variable	Data type	Description
plot_id		String		Plot ID (from transect_plots table)
year		Integer		Year of measurement
species_id	String		Species ID (from code_species table)
status_id	String		Status of stem, i.e. alive or dead with modifier (from code_status table)
dbh_class	Real		Diameter at breast height (DBH) class, denoted by midpoint of class (in cm), i.e. 0.5 for seedlings (DBH <1cm), 2.5 for saplings (DBH <5cm), 7.5 for trees 5-10cm, and so on by 5-cm increments. 
count		Integer		Number of stems with this species, status and DBH class
		

LDTRF-SEEDLINGS

Plots: seedling_plots.csv
Variable	Data type	Description
plot_id		String		Plot ID (fire year preceded by “S”)
fire_year	Integer		Year of last fire in stand
long		Real		Longitude of plot (decimal degrees)
lat		Real		Latitude of plot (decimal degrees)
		
Stems in plots: seedling_stems.csv
Variable	Data type	Description
stem_id		String		Stem ID, formed by plot ID and stem number
plot_id		String		Plot ID (from seedling_plots table)
species_id	String		Species ID (from code_species table)
x		Real		x-coordinate of stem in plot
y		Real		y-coordinate of stem in plot
		
Stem measurements in plots: seedling_stem_meas.csv
Variable		Data type	Description
meas_id			String		Measurement ID, numbered in sequence with prefix “SM” 
stem_id			String		Stem ID (from seedling_stems table)
year			Integer		Year of measurement
dbh			Real		Diameter at breast height in cm
status_id		String		Status of stem, i.e. alive or dead with modifier (from code_status table)
crown_class		String		Vertical dominance class of crown (from code_crown table)
live_crown_class	String		Live crown percentage class (from code_live_crown table)
decay_class		String		Decay class for dead trees (from code_decay table)
		
Stem injuries: seedling_stem_health.csv
Variable	Data type	Description
meas_id		String		Measurement ID (from seedling_stem_meas table)
health_id	String		Injury type (from code_health table)
location_id	String		Injury location on tree (from code_location table)
notes		String		Comments on nature of injury

Seedling subplots: seedling_subplots.csv
Variable	Data type	Description
subplot_id	String		8-character string formed by plot_id and subplot number
plot_id		String		Plot ID (from seedling_plots table)
treatment	String		Whether subplot was scarified or not (“natural”)
		
Seedling census dates: seedling_census_dates.csv
Variable	Data type	Description
plot_id		String		Plot ID (from seedling_plots table)
year		Integer		Year of seedling census
census_no	Integer		Number of census within year (1 to 4, with earliest census (1) absent before 2007)
date		String		Exact date of seedling census for subplots of that plot for that year (YYYY-MM-DD format)
		
Seedling census counts: seedling_census_counts.csv
Variable	Data type	Description
subplot_id	String	8-character string formed by plot_id and subplot number
species_id	String	Species of seedlings (from code_species table)
first_year	Integer	Year where that group of seedlings was first observed
first_census	Integer	Census number where that group of seedlings was first observed
current_year	Integer	Year for current observations
current_census	Integer	Census number for current observations
count	Integer	Count of live seedlings in group for current census
		

LDTRF-HECTARES

Plots: hectare_plots.csv
Variable	Data type	Description
plot_id		String		Plot ID (fire year preceded by “H”)
fire_year	Integer		Year of last fire in stand
long		Real		Longitude of plot (decimal degrees)
lat		Real		Latitude of plot (decimal degrees)

Stems: hectare_stems.csv
Variable	Data type	Description
stem_id		String		Stem ID, formed by plot ID and stem number
plot_id		String		Plot ID (from hectare_plots table)
species_id	String		Species ID (from code_species table)
x		Real		x-coordinate of stem in plot
y		Real		y-coordinate of stem in plot

Stem measurements: hectare_stem_meas.csv
Variable		Data type	Description
meas_id			String		Measurement ID, numbered in sequence with prefix “HM” 
stem_id			String		Stem ID (from hectare_stems table)
year			Integer		Year of measurement
dbh			Real		Diameter at breast height in cm
status_id		String		Status of stem, i.e. alive or dead with modifier (from code_status table)
crown_class		String		Vertical dominance class of crown (from code_crown table)
live_crown_class	String		Live crown percentage class (from code_live_crown table)
decay_class		String		Decay class for dead trees (from code_decay table)
		
Stem injuries: hectare_stem_health.csv
Variable	Data type	Description
meas_id		String		Measurement ID (from hectare_stem_meas table)
health_id	String		Injury type (from code_health table)
location_id	String		Injury location on tree (from code_location table)
notes		String		Comments on nature of injury

CODE DEFINITION TABLES
Seven tables (code_crown.csv, code_decay.csv, code_health.csv, code_live_crown.csv, code_location.csv, code_species.csv and code_status.csv) define the codes used in the preceding data tables. Each code table has two columns (code and description).
