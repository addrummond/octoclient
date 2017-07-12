My submission for https://github.com/circuithub/handbook/blob/master/Interviewing.rst

Some implementation decisions:

* Considers only prices quoted in USD.
* If not all parts on BOM list can be found on Octopart, gives total best
  price for all parts that can be found, together with report on BOM
  coverage.
* I wrote a CSV parser as an exercise. In production code
  I would use an existing library.

Build:

    stack build

Run:

    OCTOPART_API_KEY=aaaaaaaa ./.stack-work/install/**/bin/octoclient bom.csv 10