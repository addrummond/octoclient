My submission for https://github.com/circuithub/handbook/blob/master/Interviewing.rst

Some implementation decisions:

* Considers only prices quoted in USD.
* Does not take into account the number of different distributors
  required to get the "best" deal.
* I wrote a CSV parser as an exercise. In production code
  I would use an existing library.