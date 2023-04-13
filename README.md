# MOTE Playground

Playground for MOTE - Dashcrawl's Multiplayer Online Tile Engine

This code isn't going to be very generic, I just wanted a separate project
for dashcrawl experiments.

# Structure
lib - most recent mote library
src - clojure(script) source code for experiments
public - static files for http to serve
config - dev http server config

# Adding an experiment
1. Copy public/template.html to public/name_of_experiment.html
2. Link to html file in public/index.html
3. Copy src/experiments/template.cljs to src/experiments/name_of_experiment.cljs
4. Modify shadow-cljs.edn and add the experiment as a module
