
# Set up the repo
## Make sure $HOME/bin/ is in your $PATH variable

<details><summary>Check whether `$HOME/bin` is in your `$PATH`.</summary>

In a terminal run `$HOME/bin`, this should be `/home/your_username/bin` on
linux, and `/Users/your_username/bin` on Mac OS.

In a terminal, run `echo $PATH`, this will list a bunch of paths separated by
`:` symbols. Those are the paths where the terminal will look for commands. Is
one of them equal to `$HOME/bin`?

If your `PATH` is long, it may be difficult to do it manually. So you can run
```bash
echo $PATH | grep $HOME/bin
```

If `$HOME/bin` is in `$PATH`, the command should have returned the whole `$PATH`, possibly with the `$HOME/bin` string inside it in a different color.
You are fine, go to the next section.


If the command did not return anything, it means your `$HOME/bin` is not
in your `$PATH`, go to the next step of this section.
</details>

<details><summary>If not, add `export PATH=$HOME/bin/:$PATH` to your `.bashrc`</summary>

(only do this if you found in the previous step that $HOME/bin/ is not in your
$PATH)

`.bashrc` is a file which is run every time you open a new terminal. If you want
to have a variable every time you open a terminal, this is where you should set
that variable.

Edit the file `$HOME/.bashrc` and add the following block at the end

```bash
# Makes sure the files in `$HOME/bin/` are accessible to the command line
# In, particular, this is necessary for `pants`
export PATH=$HOME/bin/:$PATH
```
</details>

<details><summary>Make sure it worked</summary>

Open your terminal and check whether `$HOME/bin` is in your `$PATH` (step 1).

If it did not work, this means your terminal is not running your `.bashrc`. This
has happened on some Macs in the past. A possible solution could be to change
the terminal you use (`bash` instead of `zsh`). But we do not have yet a
standard fix for this, so ask for help.
</details>

## Configure DVC so it can access the remote cache
<details><summary>Add the access key of remote storage in `.dvc/config.local`</summary>

We centralize our data in
* On the Azure portal, go to the `avivadsremotedvc` storage account.
* Click "Access keys" (left panel, under "Security + Networking")
* Click "show" and then "copy" on the connection string of the most-recently
    rotated key.
* Create a file at the path `.dvc/config.local`, and write the following:
```
['remote "remotedvc"']
    connection_string = <THE_CONNECTION_STRING_YOU_COPIED>
```
* We rotate those keys, so you will have to change the connection string
    every few months
</details>

## Run the install script

From the repo's root directory:

```bash
./scripts/install/install_repo.sh
```
The script will create a virtual environment at `REPOSITORY_ROOT/venv/`. You
can activate it with `source venv/bin/activate`.

## Install git hooks
Those are commands which will be run every time you do `git checkout`, `git pull`,
`git commit` and `git push` to ensure your repository is in a good state. To
install them:

* Activate your virtual environment `source venv/bin/activate`
* Run `pre-commit install`

From now on, you will need to be in your virtual environment to do those git
commands (otherwise the hooks related to DVC will fail). If you need to force
a git command which does not pass the git hooks, you can add the `--no-verify`
option (for example, `git commit --no-verify -m "my commit message"``).


## (Optional) Configure your IDE

<details><summary>In VS Code:</summary>

### Install recommended extensions
Try
```bash
./scripts/install/install_recommended_vscode_ext.sh
```

If it does not work you can open that script, and manually look for those
extensions and install the ones which look relevant to you.

### Point VSCode to your virtualenv
* View -> Command Palette
* Start writing "Python: Select Interpreter" and select it when it appears. If
    the command is not available, check that you have the "Python" extension
    (normally installed in the previous step).
* Click on "+ Enter interpreter path"
* Click on "Find"
* Navigate to your repository's root. There should be a `venv` folder (created
    in the installation of the repository). Navigate to REPOSITORY_ROOT ->
    venv -> bin, and select the `python` file.
</details>

# Working in the repo

## Git branch

See [the guide for git workflow](docs/git_workflow.md)


## Virtual env
You should source the virtual environment (`source REPOSITORY_ROOT/venv/bin/activate`)
when working in a terminal on this repository. It is necessary to run
`git commit` because of the pre-commit hook which uses DVC, which is installed
in the virtual environment. This will also allow you to directly use `dvc`
commands, create a `cookiecutter` template and other actions requiring our dev
dependencies.



# Repo organization
* `libs/`: contains functions which are likely to be used across several projects.
 For instance, "detecting a polygon in an image". Submodules of `libs/` can not
 use submodules of `projects/`.
* `projects/`: contains functions for a specific goal, which are unlikely to be
 widely reused. For instance, "detecting a INE card in an image". Each submodule
 in `projects/` can use submodules from `lib/` but not from other submodules of
 `projects/`.


## Project and libraries structure
* Both libs and projects should have the [following structure](scripts/templates/project/{{cookiecutter.project_name}}/README.md#structure-of-the-directory), documented as a
project template.


## Starting a new project 
* Cf README from project templates for [instructions on how to use the template.](scripts/templates/README.md#how-to-use-the-template)
