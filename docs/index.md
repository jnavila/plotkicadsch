# Massaging your git for kicad

Kicad is the only electronics CAD that I know to use a nice text format for managing all the data. That feature usually fits nicely with source code version control systems such as Git. However, in spite of the text format nature of Kicad files, the fit with Git is not totally perfect and needs some ajustments for better interaction.

The following points are the setup that I reached for the edition of schematics. Caveat: most of the following tricks run on unix-like systems, but I made no attempt to port them to Windows.

## Including and Ignoring files

First off, you have to know which files you need to follow in version control and which other ones you just want the version control system to ignore.

Concerning libraries, Kicad is very helpful in that it maintains a cache library of all the components used in the schematics of a project. The presence of this file means that you don't need to check into version all the libraries from which you pulled the components, but that the project folder already contains all that is needed to open all the schematic sheets. The main files for a project named `myboard` that are followed are:

 * the `.pro` file which is main project file, e.g, `myboard.pro`
 * the `.sch` files that represent the schematic sheets.
 * the `cache.lib` which is the local cache for all the components used in your schematic, e.g. `myboard-cache.lib`
 * the `cache.dcm` which is the additional cache for component informations, e.g. `myboard-cache.dcm`

Of course, you can choose to put other files of your project, such as datasheets, simulation files, notes, documents. Don't forget that, unlike you used to do, now the upcoming versions of your board will be tagged by your version control system, so it is no use making one directory per revision of the board.

On the other side, there are files that you surely don't want to follow in version, because they are some by-products of the schematic. This encompasses typically intermediate files between the schematic and the manufacturing files.

I usually add these lines to my `.gitignore`


```bash
# export files for BOM
*.csv
*.tsv
*.xml
# backup files
 *.bak
```

## Cleaning and Smudging

Kicad generally has a nice behavior with respect to version controlling, such as taking care to not upset the structure of a schematic file when a small change is introduced. This lands very well in the version control world because when reviewing the changes between two versions, the changes are limited to some small chunks of the files. Nevertheless, some other behaviors are really annoying for version control. One of these behaviors is the fact of changing the content of the files for administrative purpose, without any causal or visual relationship with user's actions.

Fortunately, to solve this issue, there is a very handy feature of git that allows to filter the content of the files when they are about to be commited and changed back if needed when they are checked out. This feature is called cleaning (when checking in)/smudging (when checking out). Using this feature, we can force the content of certain parts of the files to remain the same in version control, even when these parts are changing in the working copy.

### How it works

The set up in Git is simple: first, we define a `filter` attribute for certain files by specifying the matching filename patterns. Then, for this type of file, we define in the config two commands that will filter the files in and out of the repository. These filter commands will accept the file content on their standard outputs and spit the modified content on their standard outputs. I will give the setup for what's needed; for more information, please refer to the documentation.

### removing the date in the .pro file

A particular one is the fact that the project file contains a field that represents the date of last modification of the project, for example, when a lib is added to the project. I don't know where this date is supposed to appear, but the changes in this field are quite disruptive for version control.

First, let's define the filter `kicad_project` for the `*.pro` files. We do so by adding a `.gitattributes` file at the root of working copy, with the following content:


```ini
*.pro filter=kicad_project
```

After that, we define the filters and we want them to be available for all the projects where the attribute is defined, so we define it at the user config level. The `clean` part of the filter (for checkin) will get rid of the date, while the `smudge` part of the filter (checkout) will do nothing.

Doing nothing is just passing the content through the `cat` command.
On the other hand, the actual filtering in the `clean` part needs a little more work. Basically, we apply a stream edition, via the `sed` command:


```console
$ git config --global filter.kicad_project.clean "sed -E 's/^update=.*$/update=Date/'"
$ git config --global filter.kicad_project.smudge cat
```

The `--global` option makes the filter available any every project where the attribute is defined for project files. From now on, changes to date in the project file won't be noticed by git.

#### Power and Flags Numbering

Another annoying behavior in Kicad Schematics is the way the power and flag parts are numbered. These components are part of the schematic but they don't appear in the BOM. So, their numbering is all managed internally by Kicad. Kicad renumbers them all everytime the user requests an annotation of the project, which modifies all those references in all the sheets each time. This feature is only really needed when generating the netlists (only for internal purpose), so it's better keeping the references for all theses phantom parts to "unknown" in the revision control system. Let's kick off another filter for that! First let's add a new attribute in the `.gitattributes` file:

```ini
*.sch filter=kicad_sch
```

Then let's clean up all the `sch` files before staging:

```console
$ git config --global filter.kicad_sch.clean "sed -E 's/#(PWR|FLG)[0-9]+/#\1?/'"
$ git config --global filter.kicad_sch.smudge cat
```

With this one, you should be able to diff your project and get a more understandable view of your changes.

## Visual Diffing

The textual diffs between revisions of a schematic sheet have cleared up a bit with the filters, but most of us poor humans don't read the schematic format in the text. To put it bluntly, except when only properties of parts are changed, the text diff is totally useless. The good news is that there is a better solution: diffing visually the schematic.

For this feature, two components are needed:

 * imagemagick's `compare` utility to compare two images, wrapped into a custom script
 * a utility I developed specially for this purpose [Plotgitsch](https://github.com/jnavila/plotkicadsch).

First, let's create the script that allows to compare two images. If the images are identical, the script just finishes, otherwise a three-pane image is displayed, showing the visual diff at the center and the revisions on each side. Here it is:

```bash
#!/bin/bash
PIPE=$(mktemp -u)
(! compare -metric RMSE $2 $1 png:${PIPE} 2> /dev/null) &&  (montage -geometry +4+4 $2 $PIPE $1 png:- | display -title "$1" -)
rm $PIPE
```

Save this file as `git-imgdiff` script, make it executable and available in your `$PATH`. You can also find it in the repo of `plotkicadsch`.

Now, `plotgitsch` can be invoked in your project's root directory in three forms:

 1. `plotgitsch rev1 rev2`, with  `rev1` and `rev2` being two references to commits ( tags, branch or any commitish form). The differing schematic sheets between the two revisions will be compared.
 2. `plotgitsch rev` performs the comparison between the working copy and the given revision. You can see quickly spot what changed since the last tagged prototype.
 3. `plotgitsch` alone, which performs the comparison between the working copy and the HEAD. This is by far the most used one. Very helpful for checking what's changed before committing.

Plotgitsch's plotting capabilities are not supposed to match those of Kicad, but to allow to quickly review the changes. The real job of comparing the two svg plots of schematic is done by the script which is quite rough, so feel free to share a better alternative for this function.


## Archiving the Project

Now that you are managing your project in Git, you can add other types of files to your project such as datasheets, BOMs and any additional files that you see fit. In this case, the Kicad archiving feature becomes less useful, it is more interesting to create your archives from Git. You benefit from the version system and you can create archive files of your project at a given revision.

Say you want a zip archive of the version 1.0 of your project. Just type:

```bash
$ git archive --format=zip --output=../myproject_v1.0.zip v1.0
```

That's all.
