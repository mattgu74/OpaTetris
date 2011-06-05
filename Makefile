
#
# OpaTetris Makefile
#
# @author Hugo Venturini
#

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.

# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


GIT?=git # used by 'make clean'
OPA?=opa #

S=@

OPA_OPTIONS=--verbose $(OPAOPT)

CONFIG_FILE=main.conf

LOGS=error.log access.log
BUILD_DIRS=_build _tracks *.opx *.opx.broken

SRC_DIR=src
SRC_FILES=$(wildcard $(SRC_DIR)/*.opa)

BIN_DIR=bin
EXEC_NAME=main.exe
EXEC=$(BIN_DIR)/$(EXEC_NAME)

INSTALL_DIR=/usr/bin/opa-tetris

all:
	$(S) mkdir -vp $(BIN_DIR)
	$(S) $(OPA) $(OPA_OPTIONS) $(CONFIG_FILE) $(SRC_FILES) -o $(EXEC)

install:
	$(S) install $(EXEC) $(INSTALL_DIR)

clean:
	$(S) rm -fvr $(BUILD_DIRS)
	$(S) rm -fvr $(LOGS)

deepclean: clean
	$(S) rm -fvr $(BIN_DIR)
	$(S) $(GIT) clean -f
