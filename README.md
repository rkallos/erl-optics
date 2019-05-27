# erl_optics

An OTP application for [optics](https://github.com/RAttab/optics), a
library to produce time series metrics.

## Requirements
-----

* Erlang OTP >= 21
* [optics](https://github.com/RAttab/optics) v1.1.10

## Building
-----
    $ rebar3 compile

## Usage
-----

For now, erl-optics only supports the [carbon backend](https://graphite.readthedocs.io/en/latest/carbon-daemons.html).
This mode requires the following parameters to be added to a config file and loaded:
```
  {erl_optics, [
      {hostname, "https://my-carbon-host.com"},
      {port, 2003},
      {mode, carbon},
      {interval, 10000}
  ]}
```

To start the application:
```
  erl_optics_app:start(),
```

To start the optics polling:
```
  erl_optics:start_optics(<<"my_namespace">>),
```
or
```
  erl_optics:start_optics(<<"my_namespace">>, Lenses),
```
if some lenses need to be declared before starting the application.

The namespace will be prepending the name of every metric, for
example: `"my_namespace.my_counter"`

The only lenses that require initial declaration are histo and
quantile. If they are unused, they can be omitted, otherwise, they
must be declared like so:

```
  Lenses = [erl_optics_lens:histo(<<"histo_name">>, [10, 20, 30, 40]),
      erl_optics_lens:quantile(<<"quantile_name">>, 0.50, 10.0, 0.5)],
  erl_optics:start_optics(<<"optics_namespace">>, Lenses).
```

## Lenses

### counter
A simple counter, polling will give the total increment value.
Increment defaults to 1.
```
  erl_optics:counter_inc(<<"my_counter_name">>),
```
or
```
  erl_optics:counter_inc(<<"my_counter_name">, 10),
```

### gauge
Will keep the last value given to it. As long as no new value has been received, it will keep sending the same.
```
  erl_optics:gauge_set(<<"my_gauge_name">>, 1.0),
```

### dist
Gives an estimation of the distribution of the values given to it. It
provides metrics for 50th, 90th and 99th percentiles, the total number
of values fed to it and the maximum value.
```
  erl_optics:dist_record(<<"my_dist_name">>, 10.0)
```
Dist can also be used to record timings with the two variants
`dist_record_timing_now` (seconds) and `dist_record_timing_now_us` (micro seconds) that
expect erlang timestamps as values and will record the difference
between the moment it is called and the timestamp receives:
```
  Time = os:timestamp(),
  % some things happen here
  erl_optics:dist_record_timing_now_us(<<"my_dist_name">>, Time),
```

### histo
Histo is a collection of buckets who are incremented every time a
value within their boundaries is received. It is necessary to declare
these lenses when starting optics:
```
  erl_optics_lens:histo(<<"my_histo_name">>, [0, 10, 20, 30])
```
To add a value to the histogram:
```
  erl_optics:histo_inc(<<"my_histo_name">>, 11.5),
```
The bucket list must be an ordered list of different integers that represent
the lower and higher boundaries of each buckets. For example, [0, 1, 2, 3] means
3 buckets: [0, 1), [1, 2) and [2, 3).


### quantile
Quantile is an estimation of the quantile value for a given percentile (e.g. if the
percentile is 0.8, and the quantile is 10.0, that means that ~80% of values were
below 10.0). It is necessary to declare these lenses when starting optics.
```
  erl_optics_lens:quantile(<<"my_quantile_name">>, <target_quantile>, <estimated_value>, <adjustment_value>)
```
- target_quantile: float, wanted percentile.
- estimated_value: expected value for given percentile
- adjustment_value: will affect the speed at which the lens converges to the correct quantile value and the precision. (e.g. an adjustement of 0.1 means that the estimated quantile will rise by 0.1 when it receives a value over itself)

## Polling
In the carbon mode, polling will happen periodically at the time interval given in the parameters. When polling occurs,
the metrics are sent at the carbon address:port via TCP in a carbon-friendly format.

If the lenses values need to be accessed directly, an erlang poller can be used instead
```
  erl_optics:register_erlang_poller(),
```
and then the polling can be done via:
```
  erl_optics:poll()
```
which will return a map containing all the lenses and their values at the moment of polling. Users should be aware that polling at irregular
time intervals may affect the merging of some metrics.

## Foil parameters
To allow dynamic use of counters, gauges and dists, the erl_optics_foil_server is triggered when they are used and will reload the foil table
after the determined foil_reload_interval.

## Environment variables
<table width="100%">
  <theader>
    <th>Name</th>
    <th>Type</th>
    <th>Default</th>
    <th>Description</th>
  </theader>
  <tr>
    <td>hostname</td>
    <td>string()</td>
    <td>"localhost"</td>
    <td>Address where the metrics will be sent in carbon mode</td>
  </tr>
  <tr>
    <td>port</td>
    <td>non_neg_integer()</td>
    <td>1055</td>
    <td>port where the metrics will be sent in carbon mode</td>
  </tr>
  <tr>
    <td>interval</td>
    <td>non_neg_integer()</td>
    <td>10000</td>
    <td>Time (in milliseconds) between polls </td>
  </tr>
  <tr>
    <td>foil_reload_interval</td>
    <td>non_neg_integer()</td>
    <td>1000</td>
    <td>Time (in milliseconds) the time a new metric is loaded and the foil table is reloaded</td>
  </tr>
  <tr>
    <td>mode</td>
    <td>carbon | blank</td>
    <td>blank</td>
    <td>Application mode</td>
  </tr>
  <tr>
    <td>foil_namespace</td>
    <td>atom()</td>
    <td>erl_optics</td>
    <td>Namespace used for the foil table</td>
  </tr>
</table>
