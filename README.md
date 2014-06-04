BeDuIn
=========

The program  is started in two steps. First you start the communication server. Then you start the number of agents needed. As the project is compiled with the Mono framework, the .exe files mentioned below can be executed with `mono <pgm>.exe`.

Start communication server:
- Open a console at the location of `NabfServerApplication.exe`
- Run `NabfServerapplication.exe <ip-address>:<port>` with an ip address and port of your choosing.
- Alternatively, run `NabfServerapplication.exe <ip-address>:<port> verbose` for debugging.

Start one agent:
- Open a console at the location of `NabfClientApplication.exe`
- Run `NabfClientapplication.exe <comm-ip>:<comm-port> <mapc-ip>:<mapc-port> <agent name> <password>`

Start several agents:
- Open `ConnectAllAgents.{bat,sh}` in a text editor.
- Modify the values used by the agents to connect to the servers.
- Place `ConnectAllAgents.{bat,sh}` in the same folder as `NabfClientApplication.exe`.
- Open a console at the location of `ConnectAllAgents.{bat,sh}`
- Run `ConnectAllAgents.{bat,sh} <min. agent nr.> <max. agent nr.>` (both inclusive)
