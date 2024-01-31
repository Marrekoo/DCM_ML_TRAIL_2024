# Discrete Choice Analysis: micro-econometrics and machine learning approaches
## *Machine learning approaches for discrete choice analysis*

- **Lecturer:** Dr. Sander van Cranenburgh
- **Teacher assistants:** Gabriel Nova

## General aim
Discrete choice analysis (DCA) has become one of the most important frameworks for transportation modelling. Using DCA, the researcher or analyst is able to estimate the influence of all sorts of factors on travel choice behaviour and to predict mobility patterns and market shares for transport-related services; all this in a quantitative, statistically rigorous way with deep roots in economics and the behavioural sciences. As such, DCA is indispensable for the underpinning of many transport policies and plans.

In this course, we will cover two different perspectives on DCA: the conventional, econometrics-based perspective (also called Discrete choice theory); and a more novel perspective which is gaining ground rapidly based on recent advances in Machine learning. This combination makes this course unique compared to other choice modelling courses taught in the Transport community.

This course will contain a mix of theory, implementation guidelines, and hands-on exercises to be completed during the course and under the supervision of the lecturer.

**This repo covers the materials for the days 3 and 4**

## Schedule

### Day 3 – Machine learning approaches for discrete choice analysis (Van Cranenburgh)
**Time**: 10.00 – 16.00 h<br>
**Room**: D2 (ground floor), TPM, Jaffalaan 5
- Introduction to machine learning for choice modellers
- Artificial neural networks & Training
- Lab session 1 (Python notebook)


### Day 4 – Machine learning approaches for discrete choice analysis (Van Cranenburgh)
**Time**: 10.00 – 16.00 h<br>
**Room**: D2 (ground floor), TPM, Jaffalaan 5
- Utility-based machine learning models (a.k.a. hybrid models)
- The L-MNL model
- Lab session 2 (Python notebook)

## Instructions to set up your Python workspace

### a. Local environment (recommended)

To get started, make sure you have Python 3.7 or a more recent version up to 3.10 installed. Additionally, ensure you have set up an IPython environment on your computer (Jupyter, VSCode, or any alternatives). 

Please follow the steps below to set up your environment.

* Step 1: Clone or download this repo to your computer (see Step 1 in the Colab section).
* Step 2: Now you have two options: (a) Install dependencies separate from your current Python version; (b) Install dependencies for this notebook in your Python version (easy way):
    * Option a: (for those familiar with Python environments):
        * Create a new "virtual environment" (a separate workspace for this course).
        * Install the required packages listed in the requirements.txt file within this environment.
        * Open the notebook you want to work on (Step 1) and make sure it's running in the newly created environment.
                         
    * Option b: (easiest way; for people unfamiliar with Python environments):
        * Open the Python notebook you want to work on (Step 1)
        * Uncomment the line related to using a local set-up and run it (see the figure below).
        * Re-comment the lines to avoid re-installing the dependencies every time you run the notebook.
          ![image](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/img_1.png)



    * Instructions for creating a new virtual environment (if choose option a):
        * Open your command prompt or terminal.
        * Navigate to the directory where you want to create the environment.
        * Type: python -m venv myenv (Replace myenv with a name you choose for your environment).
        * Activate the environment (on Windows, type: myenv\Scripts\activate, on MacOS/Linux, type: source myenv/bin/activate).
        * Install requirements from a File. With your environment activated, navigate to the folder containing the requirements.txt file and run: pip install -r requirements.txt.


### b. Google Colab 

For this option, you need a Google account,  the Google Chrome web browser, and a stable internet connection.

Please follow the steps we've included below to set up the workspace.

* Step 1: Download this repo to your computer. On the top of this site, click on (1)<>Code tab, then in the green button (2)Code and then (3) Download ZIP (See numbers 1, 2, and 3 on the following image). Unzip this file into a working folder of your own choice.

   ![image](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/img_2.png)


* Step 2: Go to http://colab.research.google.com

* Step 3: Sign in with your Google account (if you are already signed in, skip this step). If you do not have a Google account, you must (temporarily) create one.

* Step 4: Upload the Python notebook you want to work on to Colab. Click on the "Upload" tab and then on the "Choose file" tab, see numbers 1 and 2 in the figure below. Then, navigate to your working folder (Step 1) and select the Python notebook (.ipynb) you want to work on (e.g. lab_session_00.ipynb).
  ![image](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/img_3.png)
  
* Step 5: Once open, click on "View" >> "Expand sections" on the menu bar.

* Step 6: Importantly, Each notebook has a cell to prepare the data and environment in Google Colab. Uncomment (i.e. remove the '#') the lines related to the Colab set-up in your notebook, see the figure below. Run this cell and wait until finished.


 ![image](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/img_4.png)




* Step 7: The necessary packages will be loaded, don't worry about the warning messages that will appear, you're all set! You can now work on your laptop.

Finally, note that the requirements files may be updated during the course to include more dependencies if needed.
   

### c. Anaconda

If you want to use Anaconda, you must use **Python version 3.9.18 or 3.10.13**. Other versions, such as **3.7.16** and **3.8.18**, are known to give problems. Python version **3.11.5** seems to be working, but is not very stable.

##### Instruction 1: Create environment and install requirements from JupyterLab.

1. **Open the environments Tab:** Go to the "Environments" tab on the left sidebar.
   ![img1](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_1.png)

2. **Create a new environment:** Click on the "Create" button at the bottom of the window.

    ![img2](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_2.png)
   
3. **Configure the new environment:** Enter a name for your new environment, e.g., "SEN1221," and choose the Python version == “**3.10.13**" from the drop-down menu.

    ![img3](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_3.png)
   
4. **Activate the environment:** Go back to the "Home" tab. You should see your newly created environment at the right-hand side of "All applications"
 
   ![img4](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_4.png)
   
5. **Install and launch JupyterLab.**
 
   ![img5](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_5.png)
   
6. **Set up the working folder:** Download the SEN1221/Q2_2023 repo from the github repo to your computer . Unzip the file into a working folder of your own choice.

7. **Find workspace through JupyterLab:** You will see the Lab session, the `requirements.txt` file, and the `data` folder.
 
   ![img6](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_6.png)
   
8. **Uncomment and run the following cell:**

   ![img7](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_7.png)
   
9. **Import all packages**
 
   ![img8](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_8.png)
   
In case a package has not been imported, please restart the kernel and repeat steps: 8 and 9
 
   ![img9](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_9.png)
   
#### Instruction 2: Create environment and install the requirements through the terminal.

1. **Open the environments tab:** Go to the "Environments" tab on the left sidebar.

   ![img10](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_10.png)

2. **Create a new environment:** Click on the "Create" button at the bottom of the window.

   ![img11](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_11.png)

3. **Configure the new environment:** Enter a name for your new environment, e.g., "SEN1221," and choose the Python version == “3.10.13" from the drop-down menu.

   ![img12](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_12.png)

4. **Activate the environment:** Click on the green "Play" button on the right side of the environment name to open a terminal where the environment is activated.

   ![img13](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_13.png)

5. **Verify Python version:** You can run `python --version` in the terminal.

   ![img14](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_14.png)

6. **Navigate to the project folder:** Use the `cd` command to navigate to the project folder, for example, `cd …/…/…/Q2_2023`.

   ![img15](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_15.png)

7. **Install requirements file:** Now, you can install the `requirements.txt` file within the activated environment using the following command: `pip install -r requirements.txt`

   ![img16](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_16.png)
  
8. **Install and launch JupyterLab**

   ![img17](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_17.png)
   
9. **Import all packages**

   ![img18](https://github.com/DCM-ML-course-TRAIL/DCM_ML_TRAIL_2024/blob/main/Assets/Anaconda_18.png)
