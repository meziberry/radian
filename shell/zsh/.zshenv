# See .zprofile for some explanation of what is going on here.

if [ -z "$EOW_SKIP_PROFILE" ]; then
    emulate sh -c '. "$HOME/.profile"'
else
    EOW_SKIP_PROFILE=
fi
