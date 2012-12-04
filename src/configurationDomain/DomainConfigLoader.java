package configurationDomain;

import java.util.HashMap;


public abstract class DomainConfigLoader {
	
	protected HashMap<String, DomainConfig> settings;
	
	public DomainConfigLoader(HashMap<String, DomainConfig> settings) {
		this.settings = settings;
	}
	
	public void setConfig(DomainConfig config) {
		settings.put(config.getID(), config);
	}
	
	public abstract String getTag();
	public abstract DomainConfig newConfig(String type); 
	public abstract void setProperty(DomainConfig config, String property, String value);
	
}
