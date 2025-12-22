#
# Make all database files
#
URL=https://bitbucket.org/bluetooth-SIG/public

ASSIGNED_DIR = ./public/assigned_numbers
UUIDS_DIR = $(ASSIGNED_DIR)/uuids
COMPANY_DIR = $(ASSIGNED_DIR)/company_identifiers
SDP_DIR = $(ASSIGNED_DIR)/service_discovery
ATTR_DIR = $(SDP_DIR)/attribute_ids

YAML_FILES = \
	$(UUIDS_DIR)/browse_group_identifiers.yaml \
	$(UUIDS_DIR)/member_uuids.yaml \
	$(UUIDS_DIR)/sdo_uuids.yaml \
	$(UUIDS_DIR)/characteristic_uuids.yaml \
	$(UUIDS_DIR)/mesh_profile_uuids.yaml \
	$(UUIDS_DIR)/service_class.yaml \
	$(UUIDS_DIR)/declarations.yaml \
	$(UUIDS_DIR)/object_types.yaml \
	$(UUIDS_DIR)/service_uuids.yaml \
	$(UUIDS_DIR)/descriptors.yaml \
	$(UUIDS_DIR)/protocol_identifiers.yaml \
	$(UUIDS_DIR)/units.yaml \
	$(COMPANY_DIR)/company_identifiers.yaml \
	$(ATTR_DIR)/universal_attributes.yaml

YAML1_FILES = $(notdir $(YAML_FILES))

DB_FILES = $(YAML1_FILES:%.yaml=%.db)

all: $(DB_FILES)
	chmod +x $(DB_FILES)

%.db:	$(UUIDS_DIR)/%.yaml escript_db.beam
	erl -noshell -s bt_make_db start $< uuids uuid

%.db:	$(COMPANY_DIR)/%.yaml escript_db.beam
	erl -noshell -s bt_make_db start $< company_identifiers value

%.db:	$(ATTR_DIR)/%.yaml escript_db.beam
	erl -noshell -s bt_make_db start $< attribute_ids value

clean:
	rm $(DB_FILES) escript_db.beam

pull:
	git pull $(URL)

clone:
	git clone $(URL)

escript_db.beam:	escript_db.erl
	erlc escript_db.erl
