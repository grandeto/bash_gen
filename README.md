bash_gen
=====

An OTP application intended for sorting and generating 
Bash commands from a given list of predefined Bash tasks

Build
-----

    $ rebar3 compile

Run in shell
-----

    $ rebar3 shell

Test
-----

    $ rebar3 eunit

Usage
-----

- Generate a Bash command from a given list of predefined Bash tasks

```bash
curl -v -H 'Content-Type: application/json' "http://localhost:4000/generate" -d @./priv/tasks-1.json | bash

curl -v -H 'Content-Type: application/json' "http://localhost:4000/generate" -d @./priv/tasks-2.json | bash
```

- Sort a given list of predefined Bash tasks

```
POST http://localhost:4000/sort

Content-Type: application/json
```

Body example:

```json
{
    "tasks":[
        {
            "name":"task-1",
            "command":"touch file1"
        },
        {
            "name":"task-2",
            "command":"cat file1",
            "requires":[
                "task-3"
            ]
        },
        {
            "name":"task-3",
            "command":"echo 'Hello World!' > file1",
            "requires":[
                "task-1"
            ]
        },
        {
            "name":"task-4",
            "command":"rm file1",
            "requires":[
                "task-2",
                "task-3"
            ]
        }
    ]
}
```

TODO
-----

- validation service unit tests
- validation service
- final config
- e2e tests
- readme
