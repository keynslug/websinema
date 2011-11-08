Websinema
=========
A simple SNMP metrics realtime visualizer

Overview
--------

**Websinema** is a tool aimed at fulfilling specific purposes. It involves usage of simple network management protocol ([SNMP](http://en.wikipedia.org/SNMP)) to gather project-specific _metrics_. In general production grade systems usually act as a SNMP agents serving requests on handful of metrics through UDP. Unlike widespread classic systems like Cacti or something the key purpose behind Websinema is to visualize acquired metrics in a live, realtime, short-scoped manner.

Websinema consists of Erlang backend backed with [Cowboy](https://github.com/extend/cowboy) webserver and ExtJS frontend application which involves intensive usage of the Websockets to provide long-lived connections and feed data in the realtime.

Websinema support at the moment scalar SNMP objects only: counters, integers, strings, timestamps and ticks.

Building and configuring
------------------------

In order to build a project you should execute something like that:

```
git clone git://github.com/keynslug/websinema.git
cd websinema
make compile
```

The resources required by ExtJS applications are not included in the repository so the next step will be downloading of the actual ExtJS release and placing its `resources` directory under `.\priv\client\websinema\extjs`.

Then you will need to create a standard Erlang application configuration file. By default `.\default.config` is meant to be the general configuration file. Consult `example.config` to see what things you may configure.

After you have finished with configuration and added some agents you prefer to monitor execute:

```
./start-app.sh
```

That's all, the Websinema should work. Navigate to the URL you've specified in configuration file with one of the modern browsers to see if it is true.

