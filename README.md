# SensorFederator
An API written in R to federate disparate sensor data streams.

This is in the very early stages. It is designed to provide a means of having one standardised query interface and response format from a range of disparate sensors. Currently we only have soil moisture & rainfall sensors included in system, but conceptually any sensor producing a date & value timeseries could be included.

The 'Federator' is a uses a modular concept where a 'backend' is coded to make the calls to the native sensor API and present the results in a consistent output format



## Conceptual Diagram
![sensorFederator](/docs/sensorFederatorPNG.PNG)
