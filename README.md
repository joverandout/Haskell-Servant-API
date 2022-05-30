# An API written in Haskell using the Servant library

## How to run
- git clone the repository.
- In the directory, run:

```console
foo@bar:~$ stack build
```

to build the executable for the webserver.

Run the website with:
```console
foo@bar:~$ stack exec myproj-exe
```

You can view the website locally at `localhost:[port number]`, It uses a randomised port number each time, you can see where it is running in the terminal, you will get a message like this. You can just click the link rather than type out the url.
```console
Starting web server...
Started on http://localhost:43873
Press enter to quit.
```

### Installing Stack

To install Stack on Unix operating systems (including MacOS), run:
```console
foo@bar:~$ curl -sSL https://get.haskellstack.org/ | sh
```
or
```console
foo@bar:~$ wget -qO- https://get.haskellstack.org/ | sh
```

After installation, running stack setup might fail with configure: error: cannot run C compiled programs. in which case you should run:
```console
foo@bar:~$ xcode-select --install
```
Starting with macOs 10.14 (Mojave) running `xcode-select --install` might not be enough. You will need to install additional headers by running:
```console
foo@bar:~$ cd /Library/Developer/CommandLineTools/Packages/
foo@bar:~$ open macOS_SDK_headers_for_macOS_10.14.pkg
```

For further queries for specific Linux Distributions see the Stack install page:
https://docs.haskellstack.org/en/stable/install_and_upgrade/
