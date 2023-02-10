from databuilder.codes import codelist_from_csv
from databuilder.ehrql import Dataset, years
from databuilder.tables.beta.tpp import (
    medications as m,
    ons_deaths,
    patients,
    practice_registrations as r,
)

# These codelists come from eol_med_codes defined here:
# https://github.com/opensafely/deaths-at-home-covid19/blob/7dd124d4d104a83a3acb17209dc3baa6dfe3da89/analysis/codelists.py#L121-L129
dmd_codelist_names = [
    "glycopyrronium-subcutaneous-formulations",
    "haloperidol-subcutaneous-dmd",
    "hyoscine-butylbromide-subcutaneous-formulations",
    "levomepromazine-subcutaneous",
    "midazolam-end-of-life",
    "morphine-subcutaneous-dmd",
    "oxycodone-subcutaneous-dmd",
]

dmd_codelists = [
    codelist_from_csv(
        f"codelists/opensafely-{name.replace('_', '-')}.csv",
        system="dmd",
        column="dmd_id",
    )
    for name in dmd_codelist_names
]

# There's no way to combine codelists in ehrQL at the moment, so we do it manually.
dmd_codes = set.union(*(codelist.codes for codelist in dmd_codelists))

# These multilex codes correspond to three items in TPP's medications dictionary for
# "morphine sulfate injection 10mg/1ml" that don't have dm+d codes.
multilex_codes = [
    "14319;1;0",
    "14319;1;2",
    "14319;1;3",
]


dataset = Dataset()

# Set the population.  We're interested in patients who died between 1 March 2019 and 28
# Feb 2021, who were registered with a TPP practice when they died, and whose recorded
# sex was "female" or "male".
date_of_death = ons_deaths.sort_by(ons_deaths.date).last_for_patient().date
has_died = date_of_death.is_on_or_between("2019-03-01", "2021-02-28")
was_registered_at_death = (
    r.take(r.start_date <= date_of_death)
    .drop(r.end_date <= date_of_death)
    .exists_for_patient()
)
dataset.set_population(
    has_died & was_registered_at_death & patients.sex.is_in(["female", "male"])
)

# We're interested in events in two periods.
p1_date_range = ("2019-06-01", "2020-02-29")
p2_date_range = ("2020-06-01", "2021-02-28")

# The column has, for each patient, a count of medication events in period 1 with a code
# in the dm+d codelist.
dataset.dmd_p1 = (
    m.take(m.dmd_code.is_in(dmd_codes))
    .take(m.date.is_on_or_between(*p1_date_range))
    .count_for_patient()
)
# The column has, for each patient, a count of medication events in period 1 with a code
# in the multilex codelist.
dataset.multilex_p1 = (
    m.take(m.multilex_code.is_in(multilex_codes))
    .take(m.date.is_on_or_between(*p1_date_range))
    .count_for_patient()
)

# As above, for period 2.
dataset.dmd_p2 = (
    m.take(m.dmd_code.is_in(dmd_codes))
    .take(m.date.is_on_or_between(*p2_date_range))
    .count_for_patient()
)
dataset.multilex_p2 = (
    m.take(m.multilex_code.is_in(multilex_codes))
    .take(m.date.is_on_or_between(*p2_date_range))
    .count_for_patient()
)
