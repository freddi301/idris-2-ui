# Developer machine setup

This is an opinionated setup.

This setup is valid for Ubuntu and Windows.

### Install Linux

If using windows, install windows linux subsystem.

Open Windows PowerShell as administrator and run:

```PowerShell
	wsl --instal # enable Window to run a linux system
	wsl --install -d Ubuntu # install a specific linux distro
```

A restart may be required.

### Install IDE

Install [vscode](https://code.visualstudio.com/) IDE

Install extensions

- [WSL](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-wsl)
- [Idris](https://marketplace.visualstudio.com/items?itemName=j-nava.idris2-language-support)

### Install package manager

- open vscode
- open command panel (ctrl + shift + p) -> "connect to WSL using Distro"
- choose Ubuntu
- open a new terminal (ctrl + shift + p) -> "Terminal: focus next Terminal" (this will open the terminal inside linux subsytem if on windows)
- `sudo apt install chezscheme`
- install [idris2-pack](https://marketplace.visualstudio.com/items?itemName=j-nava.idris2-language-support) (if asked for "Chez Scheme executable" type "chezscheme")
