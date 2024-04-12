# AFP Project

## Setup

To run this project you need to have [cabal](https://cabal.readthedocs.io/en/stable/getting-started.html#installing-the-haskell-toolchain) and [elm](https://guide.elm-lang.org/install/elm.html) installed. Additionally, you need a working `MySql`or `MariaDB` installation. Consult a tutorial for your OS / distro to do so. In particular the `mysql-config` command should be available on your `PATH`.

For Fedroa you can for example follow the steps described [here](https://docs.fedoraproject.org/en-US/quick-docs/installing-mysql-mariadb/#_install_from_fedora_main_repo).

Before you run the server, create a database named `afp` and a user `afp` with password `pass` who is able to read and write on this database.

After making sure that the datbase is setup and reachable, you can start a development enviroment by opening two terminals, one for the frontend and one for the backend. Then run:

**Backend**
```shell
cd server
cabal run
```
**Frontend**
```shell
cd client
elm reactor
```