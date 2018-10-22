if which jenv > /dev/null; then eval "$(jenv init -)"; fi

export JAVA_HOME=$(/usr/libexec/java_home -v $(jenv version-name))
