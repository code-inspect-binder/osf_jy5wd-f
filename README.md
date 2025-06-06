# Executable Environment for OSF Project [jy5wd](https://osf.io/jy5wd/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Measuring Distinct Social Skills via Multiple Speed Assessments – A Behavior-Focused Personnel Selection Approach

**Project Description:**
> Social skills (e.g., persuading others, showing compassion, staying calm) are of key importance in work and education settings. Accordingly, the goal of many selection processes is to identify candidates who excel in desired skills. For this, high-fidelity simulations such as assessment centers (ACs) are regarded as ideal procedures because they can be used to evoke, observe, and evaluate candidates’ actual behavior. However, research has repeatedly shown that observed performance differences in ACs are not sufficiently driven by the specific skill dimensions that are defined for assessment. Building on multiple speed assessments and incorporating insights from behavioral personality science, we offer an alternative approach for the reliable and valid assessment of distinct social skills. We hereby (a) selected skills on the basis of a bottom-up analysis of observable and distinguishable interpersonal behaviors and (b) specifically designed exercises around these skills (i.e., one skill per exercise, multiple exercises per skill). Here, we present initial results of this newly developed procedure across three samples in a high-stakes selection context (N = 589). Generalizability theory analyses showed that a substantial amount of variance in assessor ratings could be attributed to the selected skills. This underlines the importance of more behaviorally focused selection procedures.

**Original OSF Page:** [https://osf.io/jy5wd/](https://osf.io/jy5wd/)

---

**Important Note:** The contents of the `jy5wd_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_jy5wd-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_jy5wd-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `jy5wd_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-jy5wd-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-jy5wd-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_jy5wd](https://github.com/code-inspect-binder/osf_jy5wd)

