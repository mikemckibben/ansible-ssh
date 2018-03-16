# ansible-ssh [![Build Status](https://travis-ci.org/mikemckibben/ansible-ssh.svg?branch=master)](https://travis-ci.org/mikemckibben/ansible-ssh)

## Overview

Utility to execute an ssh command or open an interactive login for an ansible
inventory host. The command expects the `ansible-inventory` and `ansible-config`
executables to be on the current PATH. Additionaly, `ansible-ssh` should be
executed in the ansible project directory, or the `-d` option given to point to
a valid anisble project directory containing a host inventory or dynamic
inventory script configured via `ansible.cfg`.


## Usage

```
Usage: ansible-ssh [-d|--directory DIR] HOST [ARG]
  ssh login to ansible inventory host

Available options:
  -d,--directory DIR       ansible playbook directory
  HOST                     ansible inventory hostname
  ARG                      ssh command arguments
  -h,--help                Show this help text
```

## Building

This Haskell program uses the stack build tool, see
https://docs.haskellstack.org/en/stable/README/ for installation instructions
for your platform.

The following command will install this program in the local bin directory for
your platform, see `stack path --local-bin` for the default location.
```
$ stack install
```
