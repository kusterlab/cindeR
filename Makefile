IMAGE = cinder_dev
PORT_IN = 7678
PORT_OUT = 7678
MEMORY_LIMIT = 2048mb
CPU_LIMIT = 1

build:
		docker build -t $(IMAGE) .

build_fresh:
		docker build --no-cache -t $(IMAGE) .

run: build
		docker run -d --name $(IMAGE) \
				--memory=$(MEMORY_LIMIT) \
				--cpus=$(CPU_LIMIT) \
				--restart unless-stopped \
				-p $(PORT_IN):$(PORT_IN) $(IMAGE)

clean:
		docker kill $(IMAGE)
		docker rm $(IMAGE)

jump:
		docker run -it \
		-p $(PORT_IN):$(PORT_IN) \
		$(IMAGE) bash
