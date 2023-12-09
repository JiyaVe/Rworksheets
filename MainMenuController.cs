using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.SceneManagement;

public class MainMenuController : MonoBehaviour
{
    
    // public GameObject mainMenuPanel;
    //public static bool isMainMenuActive;
    public void StartGame()
    {
        // Load the main game scene (assuming it's at build index 1)
        SceneManager.LoadScene(1);
    }

    public void RestartGame()
    {
        // Load the main game scene (assuming it's at build index 1)
        SceneManager.LoadScene(1);
    }

    public void ResumeMainMenu(GameObject mainMenuPanel, bool isMainMenuActive)
    {
        // Hide the main menu panel
        mainMenuPanel.SetActive(false);

        // Resume the main menu by setting the time scale to 1 (normal time)
        Time.timeScale = 1f;

        // Update the main menu status
        isMainMenuActive = false;
    }

    public void PauseMainMenu(GameObject mainMenuPanel, bool isMainMenuActive)
    {
        // Show the main menu panel
        mainMenuPanel.SetActive(true);

        // Pause the main menu by setting the time scale to 0 (no time)
        Time.timeScale = 0f;

        // Update the main menu status
        isMainMenuActive = true;
    }

    public void QuitGame()
    {
        // Quit the game (only works in standalone builds)
        Debug.Log("QUIT");
        Application.Quit();
    }
}

