package spockdataaccess.entity;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name = "Users")
@NamedQuery(name="countUsers", query="SELECT COUNT(u) FROM User u")
@NamedQueries({
    @NamedQuery(name="verifyUser", query="SELECT u FROM User u WHERE u.Username = :uname AND u.Password = :pword")
})
public class User implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    private String Username;
    @NotNull
    protected String Password;
    protected String Email;
    @OneToMany(mappedBy="user")
    protected Collection<UserInterface> interfaces;
    protected String accessRights;

    public User() {
        interfaces = new ArrayList<UserInterface>();
    }
    
    public String getUsername() {
        return Username;
    }

    public void setUsername(String Username) {
        this.Username = Username;
    }

    public String getPassword() {
        return Password;
    }

    public void setPassword(String Password) {
        this.Password = Password;
    }

    public String getEmail() {
        return Email;
    }

    public void setEmail(String Email) {
        this.Email = Email;
    }

    public Collection<UserInterface> getInterfaces() {
        return interfaces;
    }
    
    public void addUserInterface(UserInterface userInterface) {
        interfaces.add(userInterface);
    }
    
    public void removeUserInterface(UserInterface userInterface) {
        interfaces.remove(userInterface);
    }

    public String getAccessRights() {
        return accessRights;
    }

    public void setAccessRights(String accessRights) {
        this.accessRights = accessRights;
    }

    

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (Username != null ? Username.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof User)) {
            return false;
        }
        User other = (User) object;
        if ((this.Username == null && other.Username != null) || (this.Username != null && !this.Username.equals(other.Username))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.User[ id=" + Username + " ]";
    }
    
}
