# Roc XML parser

[![Passively maintained](https://img.shields.io/badge/maintenance-passive-yellow)](#maintenance)


## Cloning the repository

We use [Git submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules) for our dependencies. After cloning this repository please run the following command:

```
git submodule update --init --recursive
```

This will download the dependencies' code so that you can use them.


## Testing

Due to https://github.com/roc-lang/roc/issues/5654, we cannot run the package `roc test package/main.roc`. As a workaround, run the application:

```
roc test package/parse.roc
```


## Maintenance

This project is passively maintained. I intend to respond to issues and pull requests, but am not dedicating time to develop new features.