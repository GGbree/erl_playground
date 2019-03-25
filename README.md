# erl_playground

An OTP application to start coding without the boring stuff.

## Prerequisites
This project has been written for Mac and Linux environments, theoretically speaking it can run on any environment where a Erlang system is correcty installed, but consider that MS Windows and Erlang are not best buddies. Nowadays it is pretty easy to have Linux systems running in minutes using Virtual Machines, Containers, USB distro or simply double booting your laptop.

In case you use a Mac system, we strongly recommend using [homebrew](https://brew.sh/) to manage all your packages.

**OpenSSL**

Check the correct installation process for you environment.

**Erlang/OTP 19.3**

If you are on Mac, we strongly suggest using [kerl](https://github.com/kerl/kerl) to build and install the proper Erlang version on your system. For other environments you can easily find your installation package on [ErlangSolutions](https://www.erlang-solutions.com/).

## Build & Run

This is a [rebar3](https://www.rebar3.org/) project.
In order to build you can use following commands
* `./rebar3 compile`: compile the project
* `./rebar3 release`: compile the project and create a release
* `_build/default/rel/erl_playground/bin/erl_playground console`: run the project via interactive console

or you can use the **Makefile** included in the project.

## Compile GPB

Google Protocol Buffer is automatically compiled starting from the included proto file.
[Here](https://developers.google.com/protocol-buffers/) you can find all the information about it.

## What you have out of the box
This is a playgrounf application that allows you to focus on the logic of your system, rather than the boring technical stuff. It includes a basic Erlang/OTP application structure with a TCP client and a TCP server.

# Challenge: call-center
The challenge consists of implementing a call-center simulator in **Erlang/OTP19.3**. Client and server will accept **Google Protocol Buffer** messages over **TCP**.

A user will be able to call the call-center and choose among several options. Each option, identified by a numeric id, will trigger different features. **Some of them are mandatory to pass the challenge**.

## Mandatory features
### 1. Automatic responder
By connecting to the call-center, an automatic responder process will be spawned (one per user). It will respond with the list of available options.
Example:
```sh
$ Welcome to the call-center:
Press 1 to receive the weather forecast
Press 2 to receive...
```


### 2. Weather forecast
By requesting the weather forecast you will receive a **random phrase** describing a weather condition.

### 3. Answer to the Ultimate Question of Life, the Universe, and Everything
By requesting this you will get the correct answer, **no need to explain further!** :)

### 4. Operator
By choosing this option the user is connected to an operator (*echo server process*). The Operator will respond on max 3 messages, for a limited time of 10 seconds, than the communication with the operator will be closed and the user will be sent back to the Automatic Responder.

## Optional features
*These features can't be done without completing previous mandatory points*

### Limited operators
Use the [poolboy](https://github.com/devinus/poolboy) library to implement a limited number of Operators. When all Operators are busy, the user wait 5 seconds, if an Operator become available in the time span, the user is automatically connected, otherwise an apology message is sent back.

### Chat
By choosing this option the user is connected with another user that is waiting for the chat service. The chat will last until one of the two users will write **“bye”**.

### Client
Create an additional client for you application using one of the following languages:
* `C#`
* `C++`
* `Java`

### Test
Create a test suite (using Erlang Unit Test tools) to test your code.

## Delivery
To implement the challenge **you must fork this repository branch**.

When you are satisfied with your challenge, tag the commit as **1.0.0** and send us the public link of your GitHub project (*specifing the commit id as additional check*). In case the tag is after the delivery date, we’ll consider the last commit before the delivery date.
Remember to add any relevant information on the README file. If you did the optional **Client** feature remember to add the repository link on the file.

Git usage and organization will be evaluated as part of the challenge.

You have two weeks.
**Good Luck!**

# Candidate comments

In this project you can find an implementation to the mandatory challenges, the "limited operators" optional challenge and the "[client](https://github.com/ggbree/erl_playground_client)" optional challenge.

This project was tested in a VM with guest OS Centos 7 (which is compatible with erlang/OTP 19.3). Two profiles were added to the rebar3 config file, the __dedicated client__ and the __dedicated server__. An additional script was also added to the makefile for convenience, the __make reload__ script, that rebuilds both the dedicated client and server profiles.

Since different parameters gets fed inside the erlang VM for the three profiles (server, client and server with client in the same shell) it is **mandatory** to change the absolute path in the -config stanza of both the vm_server.args file and the vm_client.args file.

For example, I worked in the folder __/appserver/erl_playground__, so my config stanza for the server vm is
```
-config /appserver/erl_playground/config/server
```
Which needs to be changed in
```
-config <root project folder>/config/server
```
Nowhere in the documentation is reported how to feed a relative path as a vm argument, so that's annoying but necessary.

## Client Module

The sockclient module exposes a 
```
connect/1
```
function where the argument is the username for the connection. The module also exposes a
```
send/1
```
function that sends a bytestring to the server once a connection is established. Both functions accept an atom, list or integer as an argument. So a generic session can be initialized with
```
sockclient:connect(ggbree).
sockclient:send(<number>).
```
The module assumes that he's connecting to localhost (can be modified in the **config/client.config** file), the [client optional challenge](https://github.com/ggbree/erl_playground_client) does not.

## Server Module

The protocol structure provided is left unchanged, and the pattern matching is done on the unpacked message. The module creates the operator pool as soon as it is called and can assign operators if need be. As requested by the challenge if every operator is busy and no one is able to communicate in 5 seconds the user gets redirected to the main menu. The logic of the program was divided into two parts, one that handles the messages and one that handles the special states. The **state** parameter has information on the different states the client is on. The type **operator** is a convenience type that has all the information on the operator state and it is defined as
```
-type operator() ::
    {Module :: pid(), Worker :: pid(), Echoing :: integer(), Timeout :: integer()}.
```
I decided to handle all operator logic in the parent module sockserver, and leave the operator module with just the echo function. To me this approach is clearer because every parameter is apparent and handled in the same place.

## Dedicated client

The repository of the dedicated client can be found in

[https://github.com/ggbree/erl_playground_client](https://github.com/ggbree/erl_playground_client)

and more information can be found there.