Getting started
===============

Prerequisites
-------------

Make sure that [PostgreSQL](http://www.postgresql.org/) is properly installed
on your system and that `~/.cabal/bin/` is on your path.


Create a database
-----------------
Create a database user for your account (answer all questions with 'y'):

    sudo -u postgres createuser $USER


Create a database for use with HackageOneFive:

    createdb hackage_one_five
    psql hackage_one_five < scripts/db_layout.sql


Get tarball of latest package descriptions and feed it into the database:

    wget http://hackage.haskell.org/packages/archive/00-index.tar.gz
    runhaskell scripts/create_db.hs 00-index.tar.gz


Install and run HackageOneFive
------------------------------

    cabal install
    mkdir log
    HackageOneFive -p 8080


Point your web browser to [http://0.0.0.0:8080/](http://0.0.0.0:8080/).
