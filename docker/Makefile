SHELL := /bin/bash
.ONESHELL:

PARENT_NAME := sim-tool#$(shell basename $(dir $(PWD)))

.PHONY: .env
.env:
	@echo -------------------- $@ $$(date) --------------------
	echo "PARENT_NAME=$(PARENT_NAME)" > .env

build-docker-context:
	@echo -------------------- $@ $$(date) --------------------
	-rm -rf docker_context
	mkdir docker_context
	cp -r ide docker_context
	echo
	echo "Docker context created"

build-docker: build-docker-context .env
	@echo -------------------- $@ $$(date) --------------------
	sudo docker compose -f docker-compose.yaml -p $(PARENT_NAME) build || { exit 1; }
	rm -rf docker_context
	echo
	echo "Docker image build"

start-docker: .env
	@echo -------------------- $@ $$(date) --------------------
	sudo docker compose -f docker-compose.yaml -p $(PARENT_NAME) up -d || { exit 1; }
	echo
	echo "Docker container is running..."

shell-docker:
	@echo -------------------- $@ $$(date) --------------------
	sudo docker exec -it ide /bin/sh -c "/bin/bash && su -l rstudio" || { exit 1; }
	echo
	echo "Docker container is running..."

root-docker:
	@echo -------------------- $@ $$(date) --------------------
	sudo docker exec -u root -it ide /bin/sh -c "/bin/bash" || { exit 1; }
	echo
	echo "Docker container is running..."

stop-docker:
	@echo -------------------- $@ $$(date) --------------------
	sudo docker compose -f docker-compose.yaml -p $(PARENT_NAME) stop || { exit 1; }
	echo
	echo "Docker container is stopped"

rebuild-docker: stop-docker build-docker start-docker
