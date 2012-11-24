package interfaceDomain.swing.panels;

import interfaceDomain.swing.SwingInterface;

import java.awt.GridLayout;
import java.awt.Panel;

import javax.swing.JLabel;

public class EnvironmentPanel extends Panel {

	private static final long serialVersionUID = 251702727038216415L;
	public JLabel title;
	public JLabel envname, envtype;
	SwingInterface swingInterface;
	
	public EnvironmentPanel(SwingInterface swingInterface) {
		this.swingInterface = swingInterface;
		
		setLayout(new GridLayout(5, 1));
		
		title = new JLabel("Environment         ");
		add(title);
		envname = new JLabel("Name: ");
		add(envname);
		
	}
	
}
