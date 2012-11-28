package interfaceDomain.swing.panels;

import interfaceDomain.swing.SwingInterface;
import interfaceDomain.swing.listeners.ExitButtonListener;
import interfaceDomain.swing.listeners.StartButtonListener;

import java.awt.GridLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

import tools.errorChecking.Assert;

public class ControlsPanel extends JPanel {

	private static final long serialVersionUID = -312140961983129910L;
	SwingInterface swingInterface;
	public JButton startButton, exitButton;
	
	public ControlsPanel(SwingInterface swingInterface) {
		Assert.AssertTrue("SwingInterface correctly passed to ControlsPanel", swingInterface != null);
		this.swingInterface = swingInterface;
		
		setLayout(new GridLayout(1, 2));
		
		startButton = new JButton("Start");
        add(startButton);
        startButton.addActionListener(new StartButtonListener(swingInterface));
		
        exitButton = new JButton("Exit");
        add(exitButton);
        exitButton.addActionListener(new ExitButtonListener(swingInterface));
		
	}
	
}
