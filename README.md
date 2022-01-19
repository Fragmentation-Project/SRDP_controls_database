# SRDP common control variables database

The [Strategies of Resistance Data Project](https://journals-sagepub-com.proxy-um.researchport.umd.edu/doi/10.1177/0022343319880246) seeks to advance our understanding of how self-determination movements unfold over time, and the conditions under which self-determination organisations use conventional politics, violent tactics, non-violent tactics, or some combination of these.

This repo contains data for common control variables used to support SRDP analysis. It provides a full trail from the source data to the final data points used in this analysis. It includes both country- and group-level data.

For details of each of the variables included in the database, please see the codebook provided in the `codebook` folder. This includes a full list of data sources. Users can collect the original source data themselves by running the relevant script in the `src` folder. The scripts are organised at the country- or group-level. Our cleaning and calculation scripts can be accessed in the `munge` folder. These scripts are also organised at the country- and group-level.

## Variables

### Country-level

| Variable                                                    | First source                                             |
|-------------------------------------------------------------|----------------------------------------------------------|
| Region                                                      | World Bank                                               |
| Population                                                  | United Nations Department of Economic and Social Affairs |
| GDP                                                         | World Bank                                               |
| Military expenditure                                        | SIPRI                                                    |
| Unified Democracy Scores                                    | Unified Democracy Scores                                 |
| Freedom House political rights, civil liberties, and status | Freedom House                                            |
| Polity democracy, autocracy, and total scores               | PolityV                                                  |
| Checks and balances                                         | Database of Political Institutions                       |
| Civil war incidence, onset, and in previous year            | UCDP/PRIO Armed Conflict Dataset                         |

### Group-level

| Variable      | First source           |
|---------------|------------------------|
| Relative size | Ethnic Power Relations |
| Size          | Ethnic Power Relations |

## Future development

This repo will be converted into a package shortly to provide R users with easy access to the data.
