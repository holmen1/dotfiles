# git

| Alias | Command                               |
------------------------------------------------
| gs    | git status                            |
| ga    | git add                               |
| gcm   | git commit -m                         |
| gp=   | git push                              |
| gl    | git log --oneline --graph --decorate  |
| gco   | git checkout                          |
| gcb   | git checkout -b                       |
| gd    | git diff                              |
| gds   | git diff --staged                     |
| gpo   | git pull origin                       |
| gr    | git restore                           |
| gcl   | git clone                             |
| gsta  | git stash                             |
| gstp  | git stash pop                         |

| Description   | Command |
-------------------------
| Configure     | git config --global user.name "$git_username" |  
|               | git config --global user.email "$git_email"   |
| Behind remote | git pull --rebase                             |
| Tag           | git tag -a v1.4 -m "my version 1.4"           |
|               | git push origin v1.4                          |


### Configure Git

```bash
git config --global user.name "$git_username"  
git config --global user.email "$git_email"
```

### Repo behind remote

**Best option (recommended):**
```bash
git pull --rebase
```
Replays your local commits on top of remote changes. Linear history, cleaner.

**Alternative (preserves merge history):**
```bash
git pull --no-rebase
```
or set default:
```bash
git config pull.rebase false
```

**Or fast-forward only (fails if diverged):**
```bash
git pull --ff-only
```

**Legacy workaround (not recommended):**
```bash
git stash [-u]
git pull
git stash apply
```

### Annotated Tags

Creating an annotated tag in Git is simple. The easiest way is to specify -a when you run the tag command:
```bash
git tag -a v1.4 -m "my version 1.4"
```
By default, the git push command doesn’t transfer tags to remote servers.
You will have to explicitly push tags to a shared server after you have created them
```bash
git push origin v1.4
```

