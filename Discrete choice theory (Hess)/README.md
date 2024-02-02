# Discrete Choice Analysis: micro-econometrics and machine learning approaches

- **Lecturer:** Dr. Stephane Hess
- **Teacher assistants:** Gabriel Nova

## Schedule

### Day 1 – Basics of Discrete choice theory (Hess)
**Time**: 10.00 – 16.00 h<br>
**Room**: D2 (ground floor), TPM, Jaffalaan 5
- Introduction to choice modelling and data requirements
- Random utility and the logit model
- Model specification, estimation and interpretation of results
- Exercise 1 (Apollo)


### Day 2 – Advances in Discrete choice theory (Hess)
**Time**: 10.00 – 16.00 h<br>
**Room**: D2 (ground floor), TPM, Jaffalaan 5
- Allowing for heterogeneity in preferences
- Random heterogeneity and the mixed logit model
- Making predictions from choice models
- Exercises 2 and 3 (Apollo)

# Installing and Testing Apollo

## Install R

1. Go to [CRAN](https://cran.r-project.org/) and follow the instructions to download R for your operating system. Please note you require at least version R4.0 to run Apollo.

![img1](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Discrete%20choice%20theory%20(Hess)/assets/Picture1.png)

For example, for Windows, download the base R.

![img2](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Discrete%20choice%20theory%20(Hess)/assets/Picture2.png)

   
2. After downloading, double click on the file and install it following the on-screen instructions.

3. Depending on your operating system, applications need to be digitally signed. You may, therefore, need to unblock the setup files of both R before being able to install it.

## Install RStudio

1. Go to [RStudio](https://www.rstudio.com/) and click on "Download."

![img3](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Discrete%20choice%20theory%20(Hess)/assets/Picture3.png)


2. Scroll down and select the "Free version" for downloading.

![img4](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Discrete%20choice%20theory%20(Hess)/assets/Picture4.png)

3. Scroll down and download the installer for your operating system.

![img5](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Discrete%20choice%20theory%20(Hess)/assets/Picture5.png)


4. After downloading, double click on the file and install it following the instructions that will show up on your screen.
5. Depending on your operating system, applications need to be digitally signed. You may, therefore, need to unblock the setup files of both RStudio before being able to install it.

## Install Apollo in RStudio

1. Open RStudio.
2. In the console, type the following command:

    ```R
    install.packages("apollo")
    ```

    Press the Enter key and wait for the installation to complete.

    ![img6](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Discrete%20choice%20theory%20(Hess)/assets/Picture6.png)


3. If everything went well, you should see that at least Apollo version 0.3.1 is installed (look at the packages section, which will be in your lower right or lower left panel depending on your configuration).

   ![img7](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Discrete%20choice%20theory%20(Hess)/assets/Picture7.png)


## Run Example

4. Download the example file from [this link](https://www.dropbox.com/s/x7rqwz8bai8n5g4/MMNL.r?dl=1).
5. Open the downloaded file in RStudio (you can double click on the MMNL.r file).
6. Select all text in the script tab, and press Run.

![img8](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Discrete%20choice%20theory%20(Hess)/assets/Picture8.png)


7. After the estimation finishes, a report should be printed to the Console.

![img9](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Discrete%20choice%20theory%20(Hess)/assets/Picture9.png)


8. If the model estimated correctly (i.e., it did not generate any errors), then everything is working as it should. The runtime should not be substantially higher than in the above output.
