#!/usr/bin/env bash

#############################################################################
# This script is a result of the ideas from the people of different e       #
# channels at irc.freenode.net                                              #
# It will checkout the repository and compile e17.                          #
#                                                                           #
# License: BSD licence                                                      #
# Get the latest version at http://omicron.homeip.net/projects/#easy_e17.sh #
# Coded by Brian 'morlenxus' Miculcy (morlenxus@gmx.net)                    #
#                                                                           #
last_changes="2009-01-02"                                                   #
version="1.2.3"                                                             #
#############################################################################


# Internal variables, most are available through cmd args!
tmp_path="/tmp/easy_e17"
logs_path="$tmp_path/install_logs"
status_path="$tmp_path/status"
src_path="$HOME/e17_src"

src_url="http://svn.enlightenment.org/svn/e/trunk"
conf_files="/etc/easy_e17.conf $HOME/.easy_e17.conf ./.easy_e17.conf"

packages="imlib2 eina eet evas ecore efreet embryo edje epsilon esmart emotion etk etk_extra ewl exml enhance e_dbus exalt exchange e entrance edje_editor edje_player elicit elitaire enna enthrall emphasis empower emprint ephoto estickies exhibit expedite exquisite extrackt eyesight e_phys rage alarm bling calendar cpu deskshow diskio drawer efm_nav efm_path emu execwatch flame forecasts iiirk language mail mem moon mpdule net news notification penguins photo places rain screenshot slideshow snow taskbar tclock tiling uptime weather winselector wlan"

cmd_src_checkout="svn checkout"
cmd_src_update_conflicts_solve="svn update --accept theirs-full"
cmd_src_update_conflicts_ask="svn update"
cmd_src_test="svn info"
ignore_dirs="BINDINGS BROKEN devs DOCS E16 OLD TEST THEMES TMP web"
autogen_args=""		# evas:--enable-gl-x11
linux_distri=""		# if your distribution is wrongly detected, define it here
max_backoff=360		# Actual maximum backoff time is roughly this number in seconds.
nice_level=0		# nice level (19 == low, -20 == high)
os=$(uname)			# operating system
threads=2			# make -j <threads>

animation="star"
online_source="http://omicron.homeip.net/projects/easy_e17/easy_e17.sh"	# URL of latest stable release


#############################################################################
function logo ()
{
	clear
	echo -e "\033[1m-------------------------------\033[7m Easy_e17.sh $version \033[0m\033[1m------------------------------\033[0m"
	echo -e "\033[1m  Developers:\033[0m      Brian 'morlenxus' Miculcy"
	echo -e "                   David 'onefang' Seikel"
	echo -e "\033[1m  Contributors:\033[0m    Tim 'amon' Zebulla"
	echo -e "                   Daniel G. '_ke' Siegel"
	echo -e "                   Stefan 'slax' Langner"
	echo -e "                   Massimiliano 'Massi' Calamelli"
	echo -e "                   Thomas 'thomasg' Gstaedtner"
	echo -e "                   Roberto 'rex' Sigalotti"
	echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
	echo -e "\033[1m  Updates:\033[0m         http://omicron.homeip.net/projects/#easy_e17.sh"
	echo -e "\033[1m  Support:\033[0m         #e.de, #get-e (irc.freenode.net)"
	echo -e "                   morlenxus@gmx.net"
	echo -e "\033[1m  Patches:\033[0m         Generally accepted, please contact me!"
	echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
	echo 
	echo
	echo -e "\033[1m-----------------------------\033[7m Current Configuration \033[0m\033[1m----------------------------\033[0m"
	echo "  Install path:    $install_path"
	echo "  Source path:     $src_path"
	echo "  Source url:      $src_url"
	echo "  Logs path:       $logs_path"
	if [ "$linux_distri" ]; then
		echo "  OS:              $os (Distribution: $linux_distri)"
	else
		echo "  OS:              $os"
	fi
	echo
	echo "  Packages:        $packages"
	if [ "$skip" ]; then
		echo "  Skipping:        $skip"
	fi
	if [ "$only" ]; then
		echo "  Only:            $only"
	fi
	echo
	if [ "$skip_srcupdate" ]; then
		echo "  Skip src update: yes"
	fi
	echo -n "  Source conflict: "
	if [ "$ask_on_src_conflicts" ]; then
		echo "ask for action"
	else
		echo "solve automatically"
	fi
	if [[ clean -eq 1 ]] ; then
		echo "  Run clean:       yes"
	fi
	if [[ clean -eq 2 ]] ; then
		echo "  Run distclean:   yes"
	fi
	if [[ clean -ge 3 ]] ; then
		echo "  Run uninstall:   yes"
	fi
	if [ "$skip_errors" ]; then
		echo "  Skip errors:     yes"
	fi
	if [ "$gen_docs" ]; then
		echo "  Generate docs:   yes"
	fi
	if [ "$easy_e17_post_script" ]; then
		echo "  Post install:    $easy_e17_post_script"
	fi
	if [ "$autogen_args" ]; then
		echo "  Autogen args:    $autogen_args"
	fi
	if [ "$wait" ]; then
		echo "  Wait on exit:    yes"
	fi		
	if [ "$keep" ]; then
		echo "  Keep tempdir:    yes"
	fi
	if [ "$accache" ]; then
		echo "  Use caches:      yes"
	fi
	if [ "$threads" -ne 2 ]; then
		echo "  Threads:         $threads"
	fi
	if [ "$nice_level" -ne 0 ]; then
		echo "  Nice level:      $nice_level"
	fi	
	if [ -z "$action" ]; then
		action="MISSING!"
	fi
	echo "  Script action:   $action"
	echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
	echo
	
	if [ "$action" == "script" ]; then
		return
	fi	

	if [ $1 == 0 ]; then
		if [ "$2" ]; then
			echo -e "\033[1m-------------------------------\033[7m Bad script argument \033[0m\033[1m----------------------------\033[0m"
			echo -e "  \033[1m$2\033[0m"
		fi
	else
		echo -e "\033[1m--------------------------------\033[7m Build phase $1/3 \033[0m\033[1m-------------------------------\033[0m"
	fi

	if [ -z "$2" ]; then
		case $1 in
			0)
				if [ "$os" == "not supported" ]; then
					echo -e "\033[1m-------------------------------\033[7m Not supported OS \033[0m\033[1m------------------------------\033[0m"
				  	echo "  Your operating system '$(uname)' is not supported by this script."
					echo "  If possible please provide a patch."
				else if [ -z "$fullhelp" ]; then
					echo -e "\033[1m-----------------\033[7m Short help 'easy_e17.sh <ACTION> <OPTIONS...>' \033[0m\033[1m---------------\033[0m"
					echo "  -i, --install    = action: compile and install ALL of e17"
					echo "  -u, --update     = action: update your installed e17"
					echo "      --help       = full help"
				else
					echo -e "\033[1m-----------------\033[7m Full help 'easy_e17.sh <ACTION> <OPTIONS...>' \033[0m\033[1m----------------\033[0m"
					echo -e "  \033[1mACTION (ONLY ONE SELECTION POSSIBLE):\033[0m"
					echo "  -i, --install                       = action: compile and install ALL of e17"
					echo "  -u, --update                        = action: update your installed e17"
					echo "      --only=<name1>,<name2>,...      = action: checkout and compile ONLY the"
					echo "                                        named libs/apps"
					echo "      --srcupdate                     = update only the sources"
					echo "  -v, --check-script-version          = check for a newer release of easy_e17"
					echo "      --help                          = this help"
					echo
					echo -e "  \033[1mOPTIONS:\033[0m"
					echo "      --conf=<file>                   = use an alternate configuration file"
					echo "      --instpath=<path>               = change the default install path"
					echo "      --srcpath=<path>                = change the default source path"
					echo "      --srcurl=<url>                  = change the default source url"
					echo "      --asuser                        = do everything as the user, not as root"
					echo "      --no-sudopwd                    = sudo don't need a password..."
					echo "  -c, --clean                         = clean the sources before building"
					echo "                                        (more --cleans means more cleaning, up"
					echo "                                        to a maximum of three, which will"
					echo "                                        uninstall e17)"
					echo "  -s, --skip-srcupdate                = don't update sources"
					echo "  -a, --ask-on-src-conflicts          = ask what to do with a conflicting"
					echo "                                        source file"
					echo "      --skip=<name1>,<name2>,...      = this will skip installing the named"
					echo "                                        libs/apps"
					echo "  -d, --docs                          = generate programmers documentation"
					echo "      --postscript=<name>             = full path to a script to run as root"
					echo "                                        after installation"
					echo "  -e, --skip-errors                   = continue compiling even if there is"
					echo "                                        an error"
					echo "  -w, --wait                          = don't exit the script after finishing,"
					echo "                                        this allows 'xterm -e ./easy_e17.sh -i'"
					echo "                                        without closing the xterm"
					echo "      --anim=<animation>              = build animation:"
					echo "                                        - 'star': rotating star (default)"
					echo "                                        - 'weeh': waving man"
					echo "  -k, --keep                          = don't delete the temporary dir"
					echo
					echo "  -l, --low                           = use lowest nice level (19, slowest,"
					echo "                                        takes more time to compile, select"
					echo "                                        this if you need to work on the pc"
					echo "                                        while compiling)"
					echo "      --normal                        = default nice level ($nice_level),"
					echo "                                        will be automatically used"
					echo "  -h, --high                          = use highest nice level (-20, fastest,"
					echo "                                        slows down the pc)"
					echo "      --cache                         = Use a common configure cache and"
					echo "                                        ccache if available"
					echo "      --threads=<int>                 = 'make' can use threads, recommended on"
					echo "                                        smp systems (default: 2 threads)"
					echo "      --autogen_args=<n1>:<o1>+<o2>,. = pass some options to autogen:"
					echo "                                        <name1>:<opt1>+<opt2>,<name2>:<opt1>+..."
					echo "      --cflags=<flag1>,<flag2>,...    = pass cflags to the gcc"
					echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
					echo
					echo -e "\033[1m----------------------\033[7m Configurationfile '~/.easy_e17.conf' \033[0m\033[1m--------------------\033[0m"
					echo "  Just create this file and save your favourite arguments."
					echo "  Example: If you use a diffent source path, add this line:"
					echo "           --srcpath=$HOME/enlightenment/e17_src"
				fi fi
				;;
			1)
				echo "- running some basic system checks"
				echo "- source checkout/update"
				;;
			2)
				echo "- lib-compilation and installation"
				echo "- apps-compilation and installation"
				;;
			3)
				echo "- cleaning"
				echo "- install notes"
				;;
		esac
	fi
	echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
	echo
	echo
}

function define_os_vars ()
{
	case $os in
		Darwin)
			install_path="/opt/e17"
			make="make"
			export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/opt/local/lib/pkgconfig"
			export ACLOCAL_FLAGS="$ACLOCAL_FLAGS -I /opt/local/share/aclocal"
			export CPPFLAGS="-I/opt/local/include"
			export LDFLAGS="-Wl,-L/opt/local/lib"
			;;
		FreeBSD)
			install_path="/usr/local/e17"
			ldconfig="/sbin/ldconfig"
			make="gmake"
			export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$install_path/lib/pkgconfig"
			export ACLOCAL_FLAGS=" -I /usr/local/share/aclocal"
			export CFLAGS="$CFLAGS -lintl -liconv -L/usr/local/lib -L/usr/X11R6/lib -L$install_path/lib -I/usr/local/include -I/usr/X11R6/include -I$install_path/include"
			export CPPFLAGS="$CPPFLAGS -I/usr/local/include -I/usr/X11R6/include -I$install_path/include"
			;;
		Linux)
			install_path="/opt/e17"
			ldconfig="/sbin/ldconfig"
			make="make"
			export CFLAGS="$CFLAGS"

			if [ -z "$linux_distri" ]; then
				if [ -e "/etc/debian_version" ];	then linux_distri="debian";	fi
				if [ -e "/etc/gentoo-release" ];	then linux_distri="gentoo";	fi
				if [ -e "/etc/redhat-release" ];	then linux_distri="redhat";	fi
				if [ -e "/etc/SuSE-release" ];		then linux_distri="suse";	fi
			fi
			;;
		SunOS)
			install_path="/opt/e17"
			ldconfig="$(which crle) -u"	# there is no command like ldconfig on solaris! "crle" does nearly the same.
			make="make"
			export CFLAGS="$CFLAGS"
			;;
		*)
			os="not supported"
			logo 0
			exit
			;;
	esac
}

function find_path ()
{
	name=$1
	path=""

	for dir in `find "$src_path" -maxdepth 3 -type d -name "$name" | awk -F "$src_path/" '{print $2}'`; do
		found=0
		for idir in $ignore_dirs; do
			if [ `echo "$dir" | grep -q "$idir"; echo $?` == 0 ]; then found=1; fi
		done
		if [ $found == 1 ]; then continue; fi

		if [ "${#dir}" -lt "${#path}" ] || [ -z "$path" ]; then
			path=$dir
		fi
	done

	if [ "$path" ]; then
		echo "$src_path/$path"
	fi
}

function backoff_loop
{
	src_cmd=$1

	backoff=$(( 4 + (RANDOM % 5) ))
	attempt=1;

	while [ 1 ]; do
		$src_cmd | tee -a "$tmp_path/source_update.log"
		if [ "${PIPESTATUS[0]}" -gt 0 ]; then
			attempt=$(($attempt + 1))
			for (( i = $backoff / 2; i > 0; i-- )) do
    	        set_title "FAILED! Next attempt $attempt in $i seconds"
				echo -n -e "\rFAILED! Next attempt $attempt in \033[1m$i\033[0m seconds"
				sleep 1
			done
			echo -n -e "\r                                                            \r"
			if [[ backoff -le max_backoff ]] ; then
				backoff=$(( ($backoff * 2) + (RANDOM % 5) ))
			fi
		else
			break
		fi
	done
}

function get_src ()
{
	if [ -d "$src_path" ] && [ "`$cmd_src_test \"$src_path\" &>/dev/null; echo $?`" == 0 ]; then
		set_title "Updating sources"
		if [ "$1" ]; then
			echo "- updating sources of package '`basename $1`' ..."
			cd "$1"
		else
			echo "- updating sources (please wait, this won't output much) ..."
			cd "$src_path"
		fi
		if [ "$ask_on_src_conflicts" ]; then
			backoff_loop "$cmd_src_update_conflicts_ask"
		else
			backoff_loop "$cmd_src_update_conflicts_solve"
		fi
	else
		set_title "Checkout sources"
		echo "- checkout sources ..."
		backoff_loop "$cmd_src_checkout $src_url $src_path"
	fi
	echo
}

function build_each ()
{
	for pkg in $packages; do
		compile $pkg
	done
}

function run_command ()
{
	name=$1
	path=$2
	title=$3
	log_title=$4
	mode_needed=$5
	cmd=$6

	set_title "$name: $title ($pkg_pos/$pkg_total)"
	echo -n "$log_title"
	logfile_banner "$cmd" "$logs_path/$name.log"

	if [ $mode_needed == "rootonly" ]; then
		mode_needed=$mode
	else
		if [ $nice_level -ge 0 ]; then
			mode_needed="user"
		fi
	fi
	rm -f $status_path/$name.noerrors
	case "$mode_needed" in
		"sudo")
			echo "$sudopwd" | sudo -S nice -n $nice_level $cmd >> "$logs_path/$name.log" 2>&1 && touch $status_path/$name.noerrors &
			;;
		*)
			nice -n $nice_level $cmd >> "$logs_path/$name.log" 2>&1 && touch $status_path/$name.noerrors &
			;;
	esac	

	pid="$!"
	rotate "$pid" "$name"
}

function write_appname ()
{
	name=$1
	hidden=$2
	cnt=${#name}
	max=27

	if [ "$hidden" ]; then
		c=-3
		while [ ! $c = $cnt ]; do
			echo -n " "
			c=$(($c+1))
		done
	else
		echo -n "- $name "
	fi

	while [ ! $cnt = $max ]; do
		echo -n "."
		cnt=$(($cnt+1))
	done
	echo -n " "
}

function compile ()
{
	name=$1
	make_extra=""

	write_appname "$name"
	
	for one in $skip
	do
		if [ "$name" == "$one" ]; then
			echo "SKIPPED"
			touch $status_path/$name.skipped
			return
		fi
	done
	if [ "$only" ] || [ "$action" == "update" ]; then
		found=""
		for one in $only
		do
			if [ "$name" == "$one" ]; then
				found=1
			fi
		done
		if [ -z "$found" ]; then
			echo "SKIPPED"
			touch $status_path/$name.skipped
			return
		fi
	fi

	pkg_pos=$(($pkg_pos+1))

	if [ -e "$status_path/$name.installed" ]; then
		echo "previously installed"
		return
	fi
	path=`find_path $name`
	if [ ! -d "$path" ]; then
		echo "NOT FOUND"
		return
	fi
	cd "$path"

	rm -f $status_path/$name.noerrors
	rm -f "$logs_path/$name.log"

	if [[ clean -ge 1 ]] ; then
		if [ -e "Makefile" ]; then
			if [[ clean -eq 1 ]] ; then
				run_command "$name" "$path" "clean" "clean  : " "$mode" "$make $make_extra -j $threads clean"
				if [ ! -e "$status_path/$name.noerrors" ]; then
					if [ "$skip_errors" ]; then
						write_appname "$name" "hidden"	# clean might fail, that's ok
					else
						return
					fi
				fi
			fi
			if [[ clean -eq 2 ]] ; then
				run_command "$name" "$path" "distclean" "distcln: " "$mode" "$make $make_extra -j $threads clean distclean"
				if [ ! -e "$status_path/$name.noerrors" ]; then
					if [ "$skip_errors" ]; then
						write_appname "$name" "hidden"	# distclean might fail, that's ok
					else
						return
					fi
				fi
			fi
			if [[ clean -ge 3 ]] ; then
				run_command "$name" "$path" "uninstall" "uninst : " "rootonly" "$make $make_extra -j $threads uninstall clean distclean"
				if [ ! -e "$status_path/$name.noerrors" ] ; then return ; fi

			    # It's no longer installed if we just uninstalled it.
			    # Even if the uninstall failed, it's best to mark it as uninstalled so that a partial uninstall gets fixed later.
			    rm -f $status_path/$name.installed
			fi
		fi
	fi

	# get autogen arguments
	args=""
	for app_arg in `echo $autogen_args | tr -s '\,' ' '`
	do
		app=`echo $app_arg | cut -d':' -f1`
		if [ "$app" == "$name" ]; then
			args="$args `echo $app_arg | cut -d':' -f2- | tr -s '+' ' '`"
		fi
	done
	
	if [ -e "autogen.sh" ]; then
		run_command "$name" "$path" "autogen" "autogen: " "$mode" "./autogen.sh --prefix=$install_path $accache $args"
		if [ ! -e "$status_path/$name.noerrors" ] ; then return ; fi
	else
		if [ -e "bootstrap" ]; then
			run_command "$name" "$path" "bootstrap" "bootstrap:  " "$mode" "./bootstrap"
			if [ ! -e "$status_path/$name.noerrors" ] ; then return ; fi
			run_command "$name" "$path" "configure" "configure:  " "$mode" "./configure --prefix=$install_path $accache $args"
			if [ ! -e "$status_path/$name.noerrors" ] ; then return ; fi
		else
			if [ -e "Makefile.PL" ]; then
				run_command "$name" "$path" "perl" "perl make:  " "$mode" "perl Makefile.PL prefix=$install_path $args"
				if [ ! -e "$status_path/$name.noerrors" ] ; then return ; fi
			else
				if [ -e "Makefile" ]; then
					make_extra="PREFIX=$install_path"
				else
					echo "no build system"
					touch $status_path/$name.nobuild
					return
				fi
			fi
		fi
	fi
	
	run_command "$name" "$path" "make" "make   : " "$mode" "$make $make_extra -j $threads"
	if [ ! -e "$status_path/$name.noerrors" ] ; then return ; fi

	if [ "$gen_docs" ]; then
		if [ -e "gendoc" ]; then
			run_command "$name" "$path" "docs" "docs   : " "$mode" "sh gendoc"
			if [ ! -e "$status_path/$name.noerrors" ] ; then return ; fi
		fi
	fi

	run_command "$name" "$path" "install" "install: " "rootonly" "$make $make_extra install"
	if [ ! -e "$status_path/$name.noerrors" ] ; then return ; fi

	# All done, mark it as installed OK.
	touch $status_path/$name.installed
	rm -f $status_path/$name.noerrors
	echo "ok"
}

function rotate ()
{
	pid=$1
	name=$2
	animation_state=1
	log_line=""
	
	case $animation in
		"weeh") echo -n "     " ;;
		*)		echo -n "   " ;;
	esac
	while [ "`ps -p $pid -o comm=`" ]
	do
		last_line=`tail -1 "$logs_path/$name.log"`
		if [ ! "$log_line" = "$last_line" ]; then
			case $animation in
				"weeh")
					# waving man
					echo -e -n "\b\b\b\b\b"
					case $animation_state in
						1)
							echo -n "["
							echo -n -e "\033[1m"
							echo -n "\\o\\"
							echo -n -e "\033[0m"
							echo -n "]"
							animation_state=2
							;;
						2)
							echo -n "["
							echo -n -e "\033[1m|o|\033[0m"
							echo -n "]"
							animation_state=3
							;;
						3)
							echo -n "["
							echo -n -e "\033[1m/o/\033[0m"
							echo -n "]"
							animation_state=4
							;;
						4)
							echo -n "["
							echo -n -e "\033[1m|o|\033[0m"
							echo -n "]"
							animation_state=5
							;;
						5)
							echo -n "["
							echo -n -e "\033[1m"
							echo -n "\\o/"
							echo -n -e "\033[0m"
							echo -n "]"
							animation_state=6
							;;
						6)
							echo -n "["
							echo -n -e "\033[1m|o|\033[0m"
							echo -n "]"
							animation_state=1
							;;

					esac
					;;
				*)
					# rotating star
					echo -e -n "\b\b\b"
					case $animation_state in
						1)
							echo -n "["
							echo -n -e "\033[1m|\033[0m"
							echo -n "]"
							animation_state=2
							;;
						2)
							echo -n "["
							echo -n -e "\033[1m/\033[0m"
							echo -n "]"
							animation_state=3
							;;
						3)
							echo -n "["
							echo -n -e "\033[1m-\033[0m"
							echo -n "]"
							animation_state=4
							;;
						4)
							echo -n "["
							echo -n -e "\033[1m"
							echo -n "\\"
							echo -n -e "\033[0m"
							echo -n "]"
							animation_state=1
							;;
					esac
					;;
				esac
			log_line=$last_line
		fi
		sleep 1
	done

	if [ -e "$status_path/$name.noerrors" ]; then
		case $animation in
			"weeh")	del_lines 14 ;;
			*)		del_lines 12 ;;
		esac
	else
		case $animation in
			"weeh")	del_lines 5 ;;
			*)		del_lines 3 ;;
		esac

		echo -e "\033[1mERROR!\033[0m"

		if [ ! "$skip_errors" ]; then
        	set_title "$name: ERROR"
			echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
			echo
			echo -e "\033[1m-----------------------------------\033[7m Last loglines \033[0m\033[1m------------------------------\033[0m"
			echo -n -e "\033[1m"
			tail -25 "$logs_path/$name.log"
			echo -n -e "\033[0m"
			echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
			echo
			echo "-> Get more informations by checking the log file '$logs_path/$name.log'!"
			echo
			exit
		fi
	fi
}

function del_lines ()
{
	cnt=0
	max=$1
	while [ ! "$cnt" == "$max" ]
	do
		echo -n -e "\b \b"
		cnt=$(($cnt+1))
	done
}

function error ()
{
    set_title "ERROR: $1"
	echo -e "\n\n\033[1mERROR: $1\033[0m\n\n"
	exit 2
}

function set_title ()
{
	if [ "$1" ]; then
		message="- $1"
	fi	

	if [ "$DISPLAY" ]; then
	    case "$TERM" in
			xterm*|rxvt*|Eterm|eterm|Aterm|aterm)
	        	echo -ne "\033]0;Easy_e17.sh $message\007"
				;;
	    esac
	fi	
}

function logfile_banner ()
{
	cmd=$1
	logfile=$2
	echo "-------------------------------------------------------------------------------" >> "$logfile"
	echo "EASY_E17 $version CMD: $cmd"													   >> "$logfile"
	echo "-------------------------------------------------------------------------------" >> "$logfile"
}

function cnt_pkgs () {
    pkg_total=0
    pkg_pos=0
    
    if [ -n "$only" ]; then
        for each in $only
        do
			pkg_total=$(($pkg_total+1))
        done  
    else
        # Maybe some regexp which counts the spaces is faster?
        for each in $packages
        do
			pkg_total=$(($pkg_total+1))
        done
    fi
} 

function check_script_version ()
{
	echo "- local version .............. $version"
	echo -n "- downloading script ......... " 
	remote_version=`wget $online_source -q -U "easy_e17.sh/$version" -O - | grep -m 2 -o [0-9]\.[0-9]\.[0-9] | sort -n | head -n 1`
	if [ "$remote_version" ]; then
		echo "ok"
		echo "- remote version ............. $remote_version"	
		remote_ver=`echo "$remote_version" | tr -d '.'`
		local_ver=`echo "$version" | tr -d '.'`
		echo
		echo -n "- update available ........... "
		if [ $remote_ver -gt $local_ver ]; then
			echo -e "\033[1mYES!\033[0m"
		else
			echo "no"
		fi 
	else
		echo -e "\033[1mERROR!\033[0m"
	fi
}


# SCRIPT: 

EASY_PWD=`pwd`
set_title 
define_os_vars
accache=""
easy_options=""
command_options=$@
clean=0

# Check for alternate conf file first.
test_options=$command_options
for arg in $test_options; do
	option=`echo "'$arg'" | cut -d'=' -f1 | tr -d "'"`
	value=`echo "'$arg'" | cut -d'=' -f2- | tr -d "'"`
	if [ "$value" == "$option" ]; then
		value=""
	fi
	if [ "$option" == "--conf" ]; then
		conf_files="$conf_files $value";
	fi 
done

for file in $conf_files; do
	if [ -e "$file" ]; then
		# load configfile 
		for option in `cat "$file"`
		do
			easy_options="$easy_options $option"
		done
	fi
done

# append arguments
easy_options="$easy_options $command_options" 

# check options
for arg in $easy_options
do
	option=`echo "'$arg'" | cut -d'=' -f1 | tr -d "'"`
	value=`echo "'$arg'" | cut -d'=' -f2- | tr -d "'"`
	if [ "$value" == "$option" ]; then
		value=""
	fi

	# $action can't be set twice
	if [ "$action" ]; then 
		if [ "$option" == "-i" ] ||
		   [ "$option" == "--install" ] ||
		   [ "$option" == "-u" ] ||
		   [ "$option" == "--update" ] ||
		   [ "$option" == "--only" ] ||
		   [ "$option" == "--srcupdate" ] ||
		   [ "$option" == "-v" ] ||
		   [ "$option" == "--check-script-version" ]; then
			logo 0 "Only one action allowed! (currently using '--$action' and '$option')"
			exit
		fi
	fi
	
	case "$option" in
		"-i")						action="install" ;;
		"--install")				action="install" ;;
		"-u")						action="update" ;;
		"--update")					action="update" ;;
		"--conf")					;;
		"--only")
			if [ -z "$value" ]; then
				logo 0 "Missing value for argument '$option'!"
				exit
			fi
			action="only"
			only="`echo "$value" | tr -s '\,' '\ '` $only"
			;;
		"-v")						action="script" ;;
		"--check-script-version")	action="script" ;;
		"--srcupdate")
			action="srcupdate"
			skip="$packages"
			;;
		"--instpath")				install_path="$value" ;;
		"--srcpath")				src_path="$value" ;;
		"--srcurl")					src_url="$value" ;;
		"--asuser")					asuser=1 ;;
		"--no-sudopwd")				no_sudopwd=1 ;;
		"-c")						clean=$(( $clean + 1 ))	;;
		"--clean")					clean=$(( $clean + 1 ))	;;
		"-d")						gen_docs=1 ;;
		"--docs")					gen_docs=1 ;;
		"--postscript")				easy_e17_post_script="$value" ;;
		"-s")						skip_srcupdate=1 ;;
		"--skip-srcupdate")			skip_srcupdate=1 ;;
		"-a")						ask_on_src_conflicts=1 ;;
		"--ask-on-src-conflicts")	ask_on_src_conflicts=1 ;;
		"--skip")
			if [ -z "$value" ]; then
				logo 0 "Missing value for argument '$option'!"
				exit
			fi
			skip="`echo "$value" | tr -s '\,' '\ '` $skip"
			;;
		"-e")						skip_errors=1 ;;
		"--skip-errors")			skip_errors=1 ;;		
		"-w")						wait=1 ;;
		"--wait")					wait=1 ;;
		"--anim")
			case $value in
				"weeh")	animation="weeh" ;;
				*)		animation="star" ;;
			esac
			;;
		"-k")						keep=1 ;;
		"--keep")					keep=1 ;;

		"-l")	 					nice_level=19 ;;
		"--low") 					nice_level=19 ;;
		"--normal") ;;
		"-h")	 					nice_level=-20 ;;
		"--high") 					nice_level=-20 ;;
		"--cache")
			accache=" --cache-file=$tmp_path/easy_e17.cache"
			ccache=`whereis ccache`
			if [ ! "$ccache" = "ccache:" ]; then
			    export CC="ccache gcc"
			fi
			;;
		"--threads")
			if [ -z "$value" ] || ! expr "$value" : "[0-9]*$" >/dev/null || [ "$value" -lt 1 ]; then
				logo 0 "Missing value for argument '$option'!"
				exit
			fi
			threads=$value
			;;
		"--autogen_args")	
			if [ -z "$value" ]; then
				logo 0 "Missing value for argument '$option'!"
				exit
			fi
			autogen_args="$value"
			;;
		"--cflags")
			if [ -z "$value" ]; then
				logo 0 "Missing value for argument '$option'!"
				exit
			fi
			CFLAGS="$CFLAGS `echo "$value" | tr -s '\,' '\ '`"
			;;
		"--help")
			fullhelp=1
			logo 0
			exit
			;;
		*)
			logo 0 "Unknown argument '$option'!"
			exit
			;;
	esac
done


# Sanity check stuff if doing everything as user.
if [ "$asuser" ] && [ $nice_level -lt 0 ]; then
	nice_level=0
fi

# quit if some basic option is missing
if [ -z "$action" ] || [ -z "$install_path" ] || [ -z "$src_path" ]; then
	logo 0
	exit
fi

# check for script updates
if [ "$action" == "script" ]; then
	logo 0
	echo -e "\033[1m------------------------------\033[7m Check script version \033[0m\033[1m----------------------------\033[0m"
	check_script_version
	echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
	echo
	exit 0
fi


# run script normally
logo 1
set_title "Basic system checks"
echo -e "\033[1m-------------------------------\033[7m Basic system checks \033[0m\033[1m----------------------------\033[0m"
echo -n "- creating script dirs ....... "
mkdir -p "$tmp_path" 2>/dev/null
mkdir -p "$logs_path" 2>/dev/null
mkdir -p "$status_path" 2>/dev/null
chmod 700 "$tmp_path"
echo "ok"

max=15
for dep in automake gcc $make `echo "$cmd_src_checkout" | cut -d' ' -f1`; do
	cnt=${#dep}

    echo -n "- '$dep' available "
    while [ ! $cnt = $max ]
    do
        echo -n "."
        cnt=$(($cnt+1))
    done
    echo -n " "

	if [ `type $dep &>/dev/null; echo $?` -ne 0 ]; then
		echo -e "\033[1mNOT INSTALLED!\033[0m"
		error "Command missing!"
	else
		echo "ok"
	fi
done


if [ ! "$action"  == "srcupdate" ]; then
	echo -n "- build-user ................. "
	if [ ! "$LOGNAME" == "root" ]; then
		if [ "$asuser" ]; then
			echo "$LOGNAME (as user)"
			mode="user"
		else
			echo "$LOGNAME (non-root)"
			echo -n "- sudo available ............. "
			sudotest=`type sudo &>/dev/null ; echo $?`
			if [ "$sudotest" == 0 ]; then
				if [ "$no_sudopwd" == 1 ]; then
					echo "ok"
				else
					sudo -K
					if [ -e "$tmp_path/sudo.test" ]; then
						rm -f "$tmp_path/sudo.test"
					fi
					while [ -z "$sudopwd" ]
					do
						echo -n "enter sudo-password: "
						stty -echo
						read sudopwd
						stty echo
			
						# password check
						echo "$sudopwd" | sudo -S touch "$tmp_path/sudo.test" &>/dev/null
						if [ ! -e "$tmp_path/sudo.test" ]; then
							sudopwd=""
						fi
					done
					rm -f "$tmp_path/sudo.test"
				fi
				echo 
				mode="sudo"
			else
				error "You're not root and sudo isn't available. Please run this script as root!"
			fi
		fi
	else
		echo "root"
		mode="root"
	fi

	echo -n "- adding path to env ......... " 
	export PATH="$install_path/bin:$PATH"
	export PKG_CONFIG_PATH="$install_path/lib/pkgconfig:$PKG_CONFIG_PATH"
	export LD_LIBRARY_PATH="$install_path/lib:$LD_LIBRARY_PATH"
	echo "ok"
	
	echo -n "- checking lib-path in ldc ... "
	case $os in
		FreeBSD) ;; # placeholder
		SunOS)	 ;; # need more testing of adding libraries on different solaris versions. atm this is not working
		Linux)
			libpath="`grep -r -l -i -m 1 $install_path/lib /etc/ld.so.conf*`"
			if [ -z "$libpath" ]; then
				case $linux_distri in
					gentoo)
						e17ldcfg="/etc/env.d/40e17paths"
						echo -e "PATH=$install_path/bin\nROOTPATH=$install_path/sbin:$install_path/bin\nLDPATH=$install_path/lib\nPKG_CONFIG_PATH=$install_path/lib/pkgconfig" > $e17ldcfg 
						env-update &> /dev/null
						echo "ok (path has been added to $e17ldcfg)";
						;;

					*)
						if [ "`grep -l 'include /etc/ld.so.conf.d/' /etc/ld.so.conf`" ]; then
							e17ldcfg="/etc/ld.so.conf.d/e17.conf"
							rm $e17ldcfg 2>/dev/null
						else
							e17ldcfg="/etc/ld.so.conf";
							cp $e17ldcfg $tmp_path;
						fi

						case "$mode" in
							"user") ;;
							"root")	echo "$install_path/lib" >>$e17ldcfg ;;
							"sudo")
								echo "$install_path/lib" >> $tmp_path/`basename $e17ldcfg`
								echo "$sudopwd" | sudo -S mv -f $tmp_path/`basename $e17ldcfg` $e17ldcfg
								;;
						esac
						if [ "$asuser" ]; then
							echo "skipped (running as user)";
						else
							echo "ok (path has been added to $e17ldcfg)";
						fi
						;;
				esac
			else
				echo "ok ($libpath)";
			fi
			;;
	esac

	echo -n "- setting compile options .... "
	export CPPFLAGS="$CPPFLAGS -I$install_path/include"
	export LDFLAGS="$LDFLAGS -L$install_path/lib"
	echo "ok"
fi

echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
echo


# sources
echo -e "\033[1m-----------------------------\033[7m Source checkout/update \033[0m\033[1m---------------------------\033[0m"
if [ -z "$skip_srcupdate" ]; then
	rm "$tmp_path/source_update.log" 2>/dev/null

    if [ -n "$only" ]; then
		# install selected packages
		for each in $packages; do
			for pkg in $only; do
				if [ "$each" == "$pkg" ]; then
					path=`find_path "$pkg"`
					if [ "$path" ]; then
	                    get_src "$path"
					fi
                fi
            done
        done
    else
    	# full install    
        get_src
    fi
else
	echo -e "\n                                - - - SKIPPED - - -\n"
fi
echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
echo


if [ "$action" == "update" ] && [ -e "$tmp_path/source_update.log" ]; then
	echo -e "\033[1m--------------------------------\033[7m Parsing updates \033[0m\033[1m-------------------------------\033[0m"

	for dir in `egrep "^[A|D|G|U] " "$tmp_path/source_update.log" | \
				awk '{print $2}' | sed 's,[^/]*$,,g' | sort -u`; do
		add_pkg=""
        found=0 
		for idir in $ignore_dirs; do
			if [ `echo "$dir" | grep -q "$idir"; echo $?` == 0 ]; then found=1; fi
		done
		if [ $found == 1 ]; then continue; fi

		for pkg in $packages; do
			if [ `echo "$dir" | egrep -q "^$pkg/|/$pkg/"; echo $?` == 0 ]; then
				if [ ! `echo "$only" | egrep -q "^$pkg | $pkg\$ | $pkg "; echo $?` == 0 ]; then
					only="$pkg $only"
					echo "- $pkg"
				fi
				break
			fi
		done
	done
	
	if [ -z "$only" ]; then
		echo -e "\n                         - - - NO UPDATES AVAILABLE - - -\n"
	fi

	echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
	echo
fi

cnt_pkgs	# Count packages


echo -n "-> PREPARING FOR PHASE 2..."
set_title "Preparing for phase 2... compilation & installation"
sleep 5

logo 2
echo -e "\033[1m------------------------------\033[7m Installing packages \033[0m\033[1m-----------------------------\033[0m"
pkg_pos=0
build_each
echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
echo

# Restore current directory in case post processing wants to be pathless.
cd $EASY_PWD

echo -e "\033[1m-----------------------------\033[7m Finishing installation \033[0m\033[1m---------------------------\033[0m"
echo -n "- registering libraries ...... "
if [ -z "$asuser" ]; then
	case "$mode" in
		"sudo") echo "$sudopwd" | sudo -S nice -n $nice_level $ldconfig > /dev/null 2>&1 ;;
		*) nice -n $nice_level $ldconfig > /dev/null 2>&1 ;;
	esac
	echo "ok"
else
	echo "skipped"
fi
echo -n "- post install script ........ "
if [ "$easy_e17_post_script" ]; then
	echo -n " '$easy_e17_post_script' ... "
	case "$mode" in
		"sudo") echo "$sudopwd" | sudo -S nice -n $nice_level $easy_e17_post_script ;;
		*) nice -n $nice_level $easy_e17_post_script ;;
	esac
	echo "ok"
else	
	echo "skipped"
fi
echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
echo


echo -n "-> PREPARING FOR PHASE 3..."
set_title "Preparing for phase 3..."
sleep 5

logo 3
set_title "Finished"

for file in $logs_path/*.log ; do
	if [ "$file" == "$logs_path/*.log" ]; then
		break
	fi

	pkg=`basename "$file" | cut -d'.' -f1`
	if [ -e "$status_path/$pkg.installed" ]; then
		packages_installed="$packages_installed $pkg"
	else
		if [ -e "$status_path/$pkg.skipped" ]; then
			packages_skipped="$packages_skipped $pkg"
		else
			if [ -e "$status_path/$pkg.nobuild" ]; then
				packages_nobuild="$packages_nobuild $pkg"
			else
				packages_failed="$packages_failed $pkg"
			fi
		fi
	fi
done

echo -e "\033[1m--------------------------------\033[7m Cleaning temp dir \033[0m\033[1m-----------------------------\033[0m"
if [ -z "$keep" ]; then
	if [ "$packages_failed" ]; then
		echo -n "- saving logs ................ "	
		for pkg in $packages_installed; do
			rm "$status_path/$pkg.installed" 2>/dev/null
			rm "$logs_path/$pkg.log" 2>/dev/null
		done
	else
		echo -n "- deleting temp dir .......... "
		rm -rf $tmp_path 2>/dev/null
	fi	
	echo "ok"
else	
	echo "- saving temp dir ............ ok"
fi
echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
echo

if [ "$packages_failed" ]; then
	echo -e "\033[1m---------------------------------\033[7m Failed packages \033[0m\033[1m------------------------------\033[0m"
	for pkg in $packages_failed; do
		echo "- $pkg (error log: $logs_path/$pkg.log)"
	done
	echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
	echo 
fi		

if [ "$action" == "install" ]; then
	echo "INSTALL NOTES:"
	echo "-----------------------------------------------------------------------------"
	echo "The most incredible and really unbelievable dream has become true:"
	echo "You compiled e17 successfully!"
	echo 
	echo "Starting e17:"
	echo "Create a file ~/.xsession with the line 'exec $install_path/bin/enlightenment_start'."
	echo "Add a link to this file using 'ln -s ~/.xsession ~/.xinitrc'."
	echo
	echo "If you're using a login manager (GDM/KDM), select the session type 'default' in them."
	echo "If you're using the startx command, simply execute it now."
	echo
	echo "Note: e17 is still not released and it won't be in the near future. So don't"
	echo "ask for a stable release. e17 is still very buggy and only for experienced users"
	echo "who know what they do..."
	echo 
	echo "Rasterman didn't write this script so don't ask him for help with it."
	echo
	echo "Hint: From now on you can easily keep your installation up to date."
	echo "Simply run easy_e17.sh with -u instead of -i ."
	echo
	echo "We hope you will enjoy your trip into e17... Have fun!"
	echo -e "\033[1m--------------------------------------------------------------------------------\033[0m"
	echo
fi

# Clear this out if we ever set it.
export CC=""

# exit script or wait?
if [ "$wait" ]; then
	echo
	echo -e -n "\033[1mThe script is waiting here - simply press [enter] to exit.\033[0m"
	read
fi	

if [ "$packages_failed" ]; then
	exit 2
else
	exit 0
fi	
