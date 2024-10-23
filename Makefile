PROJECT = mud
DEPS = egre egre_mud recon

dep_egre_mud = git https://github.com/cwmaguire/egre_mud master
dep_egre = git https://github.com/cwmaguire/egre master

## copied from erlang.mk and added +native.
## Brings counterattack_behaviour test down from 1+ seconds
## to ~400ms.
ERLC_OPTS = -Werror \
						+debug_info \
						+warn_export_vars \
						+warn_shadow_vars \
						+warn_obsolete_guard
						#+'{parse_transform, egre_log_transform}'
						#+native ##\

## copied from erlang.mk
TEST_ERLC_OPTS = +debug_info \
								 +warn_export_vars \
                 +warn_shadow_vars \
								 +warn_obsolete_guard
								 #+'{parse_transform, egre_log_transform}'

include erlang.mk
