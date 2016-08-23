# Wedding Backend

## Introduction

This is the haskell backend that I hacked together in order to serve the frontend assets for my wedding website http://www.jenandgarethwedding.com.

This code was hacked together in a weekend and as such should not be used for anything more than passing interest. 

## Structure

### Domain.hs
exports the Rsvp model

### DataAccess.hs
functions for accessing the database. of note is the insertRsvp function.

### Main.hs
brings in the domain and data-access layers. starts up the webserver and implements the logic for serving the static frontend assets and handling a POST request for inserting a RSVP.
