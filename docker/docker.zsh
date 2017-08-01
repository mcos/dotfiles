docker-apply-hacks () {
    pushd ~/Library/Containers/com.docker.docker/Data/database/
    f="com.docker.driver.amd64-linux/disk/on-flush"
    git reset --hard
    echo os > $f
    git add $f && git commit -s -m "Use os (fsync) for flushing"
    popd
}
